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
%mainenv53427 = call %struct.ScmObj* @const_init_null()
%mainargs53428 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv53427, %struct.ScmObj* %mainargs53428)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv53425,%struct.ScmObj* %mainargs53426) {
%stackaddr$makeclosure53429 = alloca %struct.ScmObj*, align 8
%fptrToInt53430 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47468 to i64
%ae47468 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53430)
store volatile %struct.ScmObj* %ae47468, %struct.ScmObj** %stackaddr$makeclosure53429, align 8
%ae47469 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53431 = alloca %struct.ScmObj*, align 8
%fptrToInt53432 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47470 to i64
%ae47470 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53432)
store volatile %struct.ScmObj* %ae47470, %struct.ScmObj** %stackaddr$makeclosure53431, align 8
%argslist53424$ae474680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53433 = alloca %struct.ScmObj*, align 8
%argslist53424$ae474681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47470, %struct.ScmObj* %argslist53424$ae474680)
store volatile %struct.ScmObj* %argslist53424$ae474681, %struct.ScmObj** %stackaddr$prim53433, align 8
%stackaddr$prim53434 = alloca %struct.ScmObj*, align 8
%argslist53424$ae474682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47469, %struct.ScmObj* %argslist53424$ae474681)
store volatile %struct.ScmObj* %argslist53424$ae474682, %struct.ScmObj** %stackaddr$prim53434, align 8
%clofunc53435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47468)
musttail call tailcc void %clofunc53435(%struct.ScmObj* %ae47468, %struct.ScmObj* %argslist53424$ae474682)
ret void
}

define tailcc void @proc_clo$ae47468(%struct.ScmObj* %env$ae47468,%struct.ScmObj* %current_45args52889) {
%stackaddr$prim53436 = alloca %struct.ScmObj*, align 8
%_95k47303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52889)
store volatile %struct.ScmObj* %_95k47303, %struct.ScmObj** %stackaddr$prim53436, align 8
%stackaddr$prim53437 = alloca %struct.ScmObj*, align 8
%current_45args52890 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52889)
store volatile %struct.ScmObj* %current_45args52890, %struct.ScmObj** %stackaddr$prim53437, align 8
%stackaddr$prim53438 = alloca %struct.ScmObj*, align 8
%anf_45bind47191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52890)
store volatile %struct.ScmObj* %anf_45bind47191, %struct.ScmObj** %stackaddr$prim53438, align 8
%stackaddr$makeclosure53439 = alloca %struct.ScmObj*, align 8
%fptrToInt53440 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47483 to i64
%ae47483 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53440)
store volatile %struct.ScmObj* %ae47483, %struct.ScmObj** %stackaddr$makeclosure53439, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47483, %struct.ScmObj* %anf_45bind47191, i64 0)
%ae47484 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53441 = alloca %struct.ScmObj*, align 8
%fptrToInt53442 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47485 to i64
%ae47485 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53442)
store volatile %struct.ScmObj* %ae47485, %struct.ScmObj** %stackaddr$makeclosure53441, align 8
%argslist53419$ae474830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53443 = alloca %struct.ScmObj*, align 8
%argslist53419$ae474831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47485, %struct.ScmObj* %argslist53419$ae474830)
store volatile %struct.ScmObj* %argslist53419$ae474831, %struct.ScmObj** %stackaddr$prim53443, align 8
%stackaddr$prim53444 = alloca %struct.ScmObj*, align 8
%argslist53419$ae474832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47484, %struct.ScmObj* %argslist53419$ae474831)
store volatile %struct.ScmObj* %argslist53419$ae474832, %struct.ScmObj** %stackaddr$prim53444, align 8
%clofunc53445 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47483)
musttail call tailcc void %clofunc53445(%struct.ScmObj* %ae47483, %struct.ScmObj* %argslist53419$ae474832)
ret void
}

define tailcc void @proc_clo$ae47483(%struct.ScmObj* %env$ae47483,%struct.ScmObj* %current_45args52892) {
%stackaddr$env-ref53446 = alloca %struct.ScmObj*, align 8
%anf_45bind47191 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47483, i64 0)
store %struct.ScmObj* %anf_45bind47191, %struct.ScmObj** %stackaddr$env-ref53446
%stackaddr$prim53447 = alloca %struct.ScmObj*, align 8
%_95k47304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52892)
store volatile %struct.ScmObj* %_95k47304, %struct.ScmObj** %stackaddr$prim53447, align 8
%stackaddr$prim53448 = alloca %struct.ScmObj*, align 8
%current_45args52893 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52892)
store volatile %struct.ScmObj* %current_45args52893, %struct.ScmObj** %stackaddr$prim53448, align 8
%stackaddr$prim53449 = alloca %struct.ScmObj*, align 8
%anf_45bind47195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52893)
store volatile %struct.ScmObj* %anf_45bind47195, %struct.ScmObj** %stackaddr$prim53449, align 8
%stackaddr$makeclosure53450 = alloca %struct.ScmObj*, align 8
%fptrToInt53451 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47598 to i64
%ae47598 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53451)
store volatile %struct.ScmObj* %ae47598, %struct.ScmObj** %stackaddr$makeclosure53450, align 8
%argslist53398$anf_45bind471910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53452 = alloca %struct.ScmObj*, align 8
%argslist53398$anf_45bind471911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47195, %struct.ScmObj* %argslist53398$anf_45bind471910)
store volatile %struct.ScmObj* %argslist53398$anf_45bind471911, %struct.ScmObj** %stackaddr$prim53452, align 8
%stackaddr$prim53453 = alloca %struct.ScmObj*, align 8
%argslist53398$anf_45bind471912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47598, %struct.ScmObj* %argslist53398$anf_45bind471911)
store volatile %struct.ScmObj* %argslist53398$anf_45bind471912, %struct.ScmObj** %stackaddr$prim53453, align 8
%clofunc53454 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47191)
musttail call tailcc void %clofunc53454(%struct.ScmObj* %anf_45bind47191, %struct.ScmObj* %argslist53398$anf_45bind471912)
ret void
}

define tailcc void @proc_clo$ae47598(%struct.ScmObj* %env$ae47598,%struct.ScmObj* %current_45args52895) {
%stackaddr$prim53455 = alloca %struct.ScmObj*, align 8
%_95k47305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52895)
store volatile %struct.ScmObj* %_95k47305, %struct.ScmObj** %stackaddr$prim53455, align 8
%stackaddr$prim53456 = alloca %struct.ScmObj*, align 8
%current_45args52896 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52895)
store volatile %struct.ScmObj* %current_45args52896, %struct.ScmObj** %stackaddr$prim53456, align 8
%stackaddr$prim53457 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52896)
store volatile %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$prim53457, align 8
%stackaddr$makeclosure53458 = alloca %struct.ScmObj*, align 8
%fptrToInt53459 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47600 to i64
%ae47600 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53459)
store volatile %struct.ScmObj* %ae47600, %struct.ScmObj** %stackaddr$makeclosure53458, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47600, %struct.ScmObj* %Ycmb47069, i64 0)
%ae47601 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53460 = alloca %struct.ScmObj*, align 8
%fptrToInt53461 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47602 to i64
%ae47602 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53461)
store volatile %struct.ScmObj* %ae47602, %struct.ScmObj** %stackaddr$makeclosure53460, align 8
%argslist53397$ae476000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53462 = alloca %struct.ScmObj*, align 8
%argslist53397$ae476001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47602, %struct.ScmObj* %argslist53397$ae476000)
store volatile %struct.ScmObj* %argslist53397$ae476001, %struct.ScmObj** %stackaddr$prim53462, align 8
%stackaddr$prim53463 = alloca %struct.ScmObj*, align 8
%argslist53397$ae476002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47601, %struct.ScmObj* %argslist53397$ae476001)
store volatile %struct.ScmObj* %argslist53397$ae476002, %struct.ScmObj** %stackaddr$prim53463, align 8
%clofunc53464 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47600)
musttail call tailcc void %clofunc53464(%struct.ScmObj* %ae47600, %struct.ScmObj* %argslist53397$ae476002)
ret void
}

define tailcc void @proc_clo$ae47600(%struct.ScmObj* %env$ae47600,%struct.ScmObj* %current_45args52898) {
%stackaddr$env-ref53465 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47600, i64 0)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53465
%stackaddr$prim53466 = alloca %struct.ScmObj*, align 8
%_95k47306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52898)
store volatile %struct.ScmObj* %_95k47306, %struct.ScmObj** %stackaddr$prim53466, align 8
%stackaddr$prim53467 = alloca %struct.ScmObj*, align 8
%current_45args52899 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52898)
store volatile %struct.ScmObj* %current_45args52899, %struct.ScmObj** %stackaddr$prim53467, align 8
%stackaddr$prim53468 = alloca %struct.ScmObj*, align 8
%anf_45bind47200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52899)
store volatile %struct.ScmObj* %anf_45bind47200, %struct.ScmObj** %stackaddr$prim53468, align 8
%stackaddr$makeclosure53469 = alloca %struct.ScmObj*, align 8
%fptrToInt53470 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47678 to i64
%ae47678 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53470)
store volatile %struct.ScmObj* %ae47678, %struct.ScmObj** %stackaddr$makeclosure53469, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47678, %struct.ScmObj* %Ycmb47069, i64 0)
%argslist53381$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53471 = alloca %struct.ScmObj*, align 8
%argslist53381$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47200, %struct.ScmObj* %argslist53381$Ycmb470690)
store volatile %struct.ScmObj* %argslist53381$Ycmb470691, %struct.ScmObj** %stackaddr$prim53471, align 8
%stackaddr$prim53472 = alloca %struct.ScmObj*, align 8
%argslist53381$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47678, %struct.ScmObj* %argslist53381$Ycmb470691)
store volatile %struct.ScmObj* %argslist53381$Ycmb470692, %struct.ScmObj** %stackaddr$prim53472, align 8
%clofunc53473 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53473(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53381$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae47678(%struct.ScmObj* %env$ae47678,%struct.ScmObj* %current_45args52901) {
%stackaddr$env-ref53474 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47678, i64 0)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53474
%stackaddr$prim53475 = alloca %struct.ScmObj*, align 8
%_95k47307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52901)
store volatile %struct.ScmObj* %_95k47307, %struct.ScmObj** %stackaddr$prim53475, align 8
%stackaddr$prim53476 = alloca %struct.ScmObj*, align 8
%current_45args52902 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52901)
store volatile %struct.ScmObj* %current_45args52902, %struct.ScmObj** %stackaddr$prim53476, align 8
%stackaddr$prim53477 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52902)
store volatile %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$prim53477, align 8
%stackaddr$makeclosure53478 = alloca %struct.ScmObj*, align 8
%fptrToInt53479 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47680 to i64
%ae47680 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53479)
store volatile %struct.ScmObj* %ae47680, %struct.ScmObj** %stackaddr$makeclosure53478, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47680, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47680, %struct.ScmObj* %Ycmb47069, i64 1)
%ae47681 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53480 = alloca %struct.ScmObj*, align 8
%fptrToInt53481 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47682 to i64
%ae47682 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53481)
store volatile %struct.ScmObj* %ae47682, %struct.ScmObj** %stackaddr$makeclosure53480, align 8
%argslist53380$ae476800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53482 = alloca %struct.ScmObj*, align 8
%argslist53380$ae476801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47682, %struct.ScmObj* %argslist53380$ae476800)
store volatile %struct.ScmObj* %argslist53380$ae476801, %struct.ScmObj** %stackaddr$prim53482, align 8
%stackaddr$prim53483 = alloca %struct.ScmObj*, align 8
%argslist53380$ae476802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47681, %struct.ScmObj* %argslist53380$ae476801)
store volatile %struct.ScmObj* %argslist53380$ae476802, %struct.ScmObj** %stackaddr$prim53483, align 8
%clofunc53484 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47680)
musttail call tailcc void %clofunc53484(%struct.ScmObj* %ae47680, %struct.ScmObj* %argslist53380$ae476802)
ret void
}

define tailcc void @proc_clo$ae47680(%struct.ScmObj* %env$ae47680,%struct.ScmObj* %current_45args52904) {
%stackaddr$env-ref53485 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47680, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53485
%stackaddr$env-ref53486 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47680, i64 1)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53486
%stackaddr$prim53487 = alloca %struct.ScmObj*, align 8
%_95k47308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52904)
store volatile %struct.ScmObj* %_95k47308, %struct.ScmObj** %stackaddr$prim53487, align 8
%stackaddr$prim53488 = alloca %struct.ScmObj*, align 8
%current_45args52905 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52904)
store volatile %struct.ScmObj* %current_45args52905, %struct.ScmObj** %stackaddr$prim53488, align 8
%stackaddr$prim53489 = alloca %struct.ScmObj*, align 8
%anf_45bind47206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52905)
store volatile %struct.ScmObj* %anf_45bind47206, %struct.ScmObj** %stackaddr$prim53489, align 8
%stackaddr$makeclosure53490 = alloca %struct.ScmObj*, align 8
%fptrToInt53491 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47775 to i64
%ae47775 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53491)
store volatile %struct.ScmObj* %ae47775, %struct.ScmObj** %stackaddr$makeclosure53490, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47775, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47775, %struct.ScmObj* %Ycmb47069, i64 1)
%argslist53361$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53492 = alloca %struct.ScmObj*, align 8
%argslist53361$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47206, %struct.ScmObj* %argslist53361$Ycmb470690)
store volatile %struct.ScmObj* %argslist53361$Ycmb470691, %struct.ScmObj** %stackaddr$prim53492, align 8
%stackaddr$prim53493 = alloca %struct.ScmObj*, align 8
%argslist53361$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47775, %struct.ScmObj* %argslist53361$Ycmb470691)
store volatile %struct.ScmObj* %argslist53361$Ycmb470692, %struct.ScmObj** %stackaddr$prim53493, align 8
%clofunc53494 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53494(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53361$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae47775(%struct.ScmObj* %env$ae47775,%struct.ScmObj* %current_45args52907) {
%stackaddr$env-ref53495 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47775, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53495
%stackaddr$env-ref53496 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47775, i64 1)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53496
%stackaddr$prim53497 = alloca %struct.ScmObj*, align 8
%_95k47309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52907)
store volatile %struct.ScmObj* %_95k47309, %struct.ScmObj** %stackaddr$prim53497, align 8
%stackaddr$prim53498 = alloca %struct.ScmObj*, align 8
%current_45args52908 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52907)
store volatile %struct.ScmObj* %current_45args52908, %struct.ScmObj** %stackaddr$prim53498, align 8
%stackaddr$prim53499 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52908)
store volatile %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$prim53499, align 8
%stackaddr$makeclosure53500 = alloca %struct.ScmObj*, align 8
%fptrToInt53501 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47777 to i64
%ae47777 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53501)
store volatile %struct.ScmObj* %ae47777, %struct.ScmObj** %stackaddr$makeclosure53500, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47777, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47777, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47777, %struct.ScmObj* %Ycmb47069, i64 2)
%ae47778 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53502 = alloca %struct.ScmObj*, align 8
%fptrToInt53503 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47779 to i64
%ae47779 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53503)
store volatile %struct.ScmObj* %ae47779, %struct.ScmObj** %stackaddr$makeclosure53502, align 8
%argslist53360$ae477770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53504 = alloca %struct.ScmObj*, align 8
%argslist53360$ae477771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47779, %struct.ScmObj* %argslist53360$ae477770)
store volatile %struct.ScmObj* %argslist53360$ae477771, %struct.ScmObj** %stackaddr$prim53504, align 8
%stackaddr$prim53505 = alloca %struct.ScmObj*, align 8
%argslist53360$ae477772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47778, %struct.ScmObj* %argslist53360$ae477771)
store volatile %struct.ScmObj* %argslist53360$ae477772, %struct.ScmObj** %stackaddr$prim53505, align 8
%clofunc53506 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47777)
musttail call tailcc void %clofunc53506(%struct.ScmObj* %ae47777, %struct.ScmObj* %argslist53360$ae477772)
ret void
}

define tailcc void @proc_clo$ae47777(%struct.ScmObj* %env$ae47777,%struct.ScmObj* %current_45args52910) {
%stackaddr$env-ref53507 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47777, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53507
%stackaddr$env-ref53508 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47777, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53508
%stackaddr$env-ref53509 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47777, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53509
%stackaddr$prim53510 = alloca %struct.ScmObj*, align 8
%_95k47310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52910)
store volatile %struct.ScmObj* %_95k47310, %struct.ScmObj** %stackaddr$prim53510, align 8
%stackaddr$prim53511 = alloca %struct.ScmObj*, align 8
%current_45args52911 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52910)
store volatile %struct.ScmObj* %current_45args52911, %struct.ScmObj** %stackaddr$prim53511, align 8
%stackaddr$prim53512 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52911)
store volatile %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$prim53512, align 8
%stackaddr$makeclosure53513 = alloca %struct.ScmObj*, align 8
%fptrToInt53514 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47925 to i64
%ae47925 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53514)
store volatile %struct.ScmObj* %ae47925, %struct.ScmObj** %stackaddr$makeclosure53513, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47925, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47925, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47925, %struct.ScmObj* %Ycmb47069, i64 2)
%argslist53344$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53515 = alloca %struct.ScmObj*, align 8
%argslist53344$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47213, %struct.ScmObj* %argslist53344$Ycmb470690)
store volatile %struct.ScmObj* %argslist53344$Ycmb470691, %struct.ScmObj** %stackaddr$prim53515, align 8
%stackaddr$prim53516 = alloca %struct.ScmObj*, align 8
%argslist53344$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47925, %struct.ScmObj* %argslist53344$Ycmb470691)
store volatile %struct.ScmObj* %argslist53344$Ycmb470692, %struct.ScmObj** %stackaddr$prim53516, align 8
%clofunc53517 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53517(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53344$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae47925(%struct.ScmObj* %env$ae47925,%struct.ScmObj* %current_45args52913) {
%stackaddr$env-ref53518 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47925, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53518
%stackaddr$env-ref53519 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47925, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53519
%stackaddr$env-ref53520 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47925, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53520
%stackaddr$prim53521 = alloca %struct.ScmObj*, align 8
%_95k47311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52913)
store volatile %struct.ScmObj* %_95k47311, %struct.ScmObj** %stackaddr$prim53521, align 8
%stackaddr$prim53522 = alloca %struct.ScmObj*, align 8
%current_45args52914 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52913)
store volatile %struct.ScmObj* %current_45args52914, %struct.ScmObj** %stackaddr$prim53522, align 8
%stackaddr$prim53523 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52914)
store volatile %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$prim53523, align 8
%stackaddr$makeclosure53524 = alloca %struct.ScmObj*, align 8
%fptrToInt53525 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47927 to i64
%ae47927 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53525)
store volatile %struct.ScmObj* %ae47927, %struct.ScmObj** %stackaddr$makeclosure53524, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47927, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47927, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47927, %struct.ScmObj* %Ycmb47069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47927, %struct.ScmObj* %_37take47082, i64 3)
%ae47928 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53526 = alloca %struct.ScmObj*, align 8
%fptrToInt53527 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47929 to i64
%ae47929 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53527)
store volatile %struct.ScmObj* %ae47929, %struct.ScmObj** %stackaddr$makeclosure53526, align 8
%argslist53343$ae479270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53528 = alloca %struct.ScmObj*, align 8
%argslist53343$ae479271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47929, %struct.ScmObj* %argslist53343$ae479270)
store volatile %struct.ScmObj* %argslist53343$ae479271, %struct.ScmObj** %stackaddr$prim53528, align 8
%stackaddr$prim53529 = alloca %struct.ScmObj*, align 8
%argslist53343$ae479272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47928, %struct.ScmObj* %argslist53343$ae479271)
store volatile %struct.ScmObj* %argslist53343$ae479272, %struct.ScmObj** %stackaddr$prim53529, align 8
%clofunc53530 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47927)
musttail call tailcc void %clofunc53530(%struct.ScmObj* %ae47927, %struct.ScmObj* %argslist53343$ae479272)
ret void
}

define tailcc void @proc_clo$ae47927(%struct.ScmObj* %env$ae47927,%struct.ScmObj* %current_45args52916) {
%stackaddr$env-ref53531 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47927, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53531
%stackaddr$env-ref53532 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47927, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53532
%stackaddr$env-ref53533 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47927, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53533
%stackaddr$env-ref53534 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47927, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref53534
%stackaddr$prim53535 = alloca %struct.ScmObj*, align 8
%_95k47312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52916)
store volatile %struct.ScmObj* %_95k47312, %struct.ScmObj** %stackaddr$prim53535, align 8
%stackaddr$prim53536 = alloca %struct.ScmObj*, align 8
%current_45args52917 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52916)
store volatile %struct.ScmObj* %current_45args52917, %struct.ScmObj** %stackaddr$prim53536, align 8
%stackaddr$prim53537 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52917)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim53537, align 8
%stackaddr$makeclosure53538 = alloca %struct.ScmObj*, align 8
%fptrToInt53539 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48008 to i64
%ae48008 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53539)
store volatile %struct.ScmObj* %ae48008, %struct.ScmObj** %stackaddr$makeclosure53538, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48008, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48008, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48008, %struct.ScmObj* %Ycmb47069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48008, %struct.ScmObj* %_37take47082, i64 3)
%argslist53329$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53540 = alloca %struct.ScmObj*, align 8
%argslist53329$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47217, %struct.ScmObj* %argslist53329$Ycmb470690)
store volatile %struct.ScmObj* %argslist53329$Ycmb470691, %struct.ScmObj** %stackaddr$prim53540, align 8
%stackaddr$prim53541 = alloca %struct.ScmObj*, align 8
%argslist53329$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48008, %struct.ScmObj* %argslist53329$Ycmb470691)
store volatile %struct.ScmObj* %argslist53329$Ycmb470692, %struct.ScmObj** %stackaddr$prim53541, align 8
%clofunc53542 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53542(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53329$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae48008(%struct.ScmObj* %env$ae48008,%struct.ScmObj* %current_45args52919) {
%stackaddr$env-ref53543 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48008, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53543
%stackaddr$env-ref53544 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48008, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53544
%stackaddr$env-ref53545 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48008, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53545
%stackaddr$env-ref53546 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48008, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref53546
%stackaddr$prim53547 = alloca %struct.ScmObj*, align 8
%_95k47313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52919)
store volatile %struct.ScmObj* %_95k47313, %struct.ScmObj** %stackaddr$prim53547, align 8
%stackaddr$prim53548 = alloca %struct.ScmObj*, align 8
%current_45args52920 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52919)
store volatile %struct.ScmObj* %current_45args52920, %struct.ScmObj** %stackaddr$prim53548, align 8
%stackaddr$prim53549 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52920)
store volatile %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$prim53549, align 8
%stackaddr$makeclosure53550 = alloca %struct.ScmObj*, align 8
%fptrToInt53551 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48010 to i64
%ae48010 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53551)
store volatile %struct.ScmObj* %ae48010, %struct.ScmObj** %stackaddr$makeclosure53550, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48010, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48010, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48010, %struct.ScmObj* %Ycmb47069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48010, %struct.ScmObj* %_37take47082, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48010, %struct.ScmObj* %_37length47079, i64 4)
%ae48011 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53552 = alloca %struct.ScmObj*, align 8
%fptrToInt53553 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48012 to i64
%ae48012 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53553)
store volatile %struct.ScmObj* %ae48012, %struct.ScmObj** %stackaddr$makeclosure53552, align 8
%argslist53328$ae480100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53554 = alloca %struct.ScmObj*, align 8
%argslist53328$ae480101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48012, %struct.ScmObj* %argslist53328$ae480100)
store volatile %struct.ScmObj* %argslist53328$ae480101, %struct.ScmObj** %stackaddr$prim53554, align 8
%stackaddr$prim53555 = alloca %struct.ScmObj*, align 8
%argslist53328$ae480102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48011, %struct.ScmObj* %argslist53328$ae480101)
store volatile %struct.ScmObj* %argslist53328$ae480102, %struct.ScmObj** %stackaddr$prim53555, align 8
%clofunc53556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48010)
musttail call tailcc void %clofunc53556(%struct.ScmObj* %ae48010, %struct.ScmObj* %argslist53328$ae480102)
ret void
}

define tailcc void @proc_clo$ae48010(%struct.ScmObj* %env$ae48010,%struct.ScmObj* %current_45args52922) {
%stackaddr$env-ref53557 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48010, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53557
%stackaddr$env-ref53558 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48010, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53558
%stackaddr$env-ref53559 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48010, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53559
%stackaddr$env-ref53560 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48010, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref53560
%stackaddr$env-ref53561 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48010, i64 4)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref53561
%stackaddr$prim53562 = alloca %struct.ScmObj*, align 8
%_95k47314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52922)
store volatile %struct.ScmObj* %_95k47314, %struct.ScmObj** %stackaddr$prim53562, align 8
%stackaddr$prim53563 = alloca %struct.ScmObj*, align 8
%current_45args52923 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52922)
store volatile %struct.ScmObj* %current_45args52923, %struct.ScmObj** %stackaddr$prim53563, align 8
%stackaddr$prim53564 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52923)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim53564, align 8
%stackaddr$makeclosure53565 = alloca %struct.ScmObj*, align 8
%fptrToInt53566 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48087 to i64
%ae48087 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53566)
store volatile %struct.ScmObj* %ae48087, %struct.ScmObj** %stackaddr$makeclosure53565, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48087, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48087, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48087, %struct.ScmObj* %Ycmb47069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48087, %struct.ScmObj* %_37take47082, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48087, %struct.ScmObj* %_37length47079, i64 4)
%argslist53312$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53567 = alloca %struct.ScmObj*, align 8
%argslist53312$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47222, %struct.ScmObj* %argslist53312$Ycmb470690)
store volatile %struct.ScmObj* %argslist53312$Ycmb470691, %struct.ScmObj** %stackaddr$prim53567, align 8
%stackaddr$prim53568 = alloca %struct.ScmObj*, align 8
%argslist53312$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48087, %struct.ScmObj* %argslist53312$Ycmb470691)
store volatile %struct.ScmObj* %argslist53312$Ycmb470692, %struct.ScmObj** %stackaddr$prim53568, align 8
%clofunc53569 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53569(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53312$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae48087(%struct.ScmObj* %env$ae48087,%struct.ScmObj* %current_45args52925) {
%stackaddr$env-ref53570 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48087, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53570
%stackaddr$env-ref53571 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48087, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53571
%stackaddr$env-ref53572 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48087, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53572
%stackaddr$env-ref53573 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48087, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref53573
%stackaddr$env-ref53574 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48087, i64 4)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref53574
%stackaddr$prim53575 = alloca %struct.ScmObj*, align 8
%_95k47315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52925)
store volatile %struct.ScmObj* %_95k47315, %struct.ScmObj** %stackaddr$prim53575, align 8
%stackaddr$prim53576 = alloca %struct.ScmObj*, align 8
%current_45args52926 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52925)
store volatile %struct.ScmObj* %current_45args52926, %struct.ScmObj** %stackaddr$prim53576, align 8
%stackaddr$prim53577 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52926)
store volatile %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$prim53577, align 8
%stackaddr$makeclosure53578 = alloca %struct.ScmObj*, align 8
%fptrToInt53579 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48089 to i64
%ae48089 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53579)
store volatile %struct.ScmObj* %ae48089, %struct.ScmObj** %stackaddr$makeclosure53578, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48089, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48089, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48089, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48089, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48089, %struct.ScmObj* %_37take47082, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48089, %struct.ScmObj* %_37length47079, i64 5)
%ae48090 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53580 = alloca %struct.ScmObj*, align 8
%fptrToInt53581 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48091 to i64
%ae48091 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53581)
store volatile %struct.ScmObj* %ae48091, %struct.ScmObj** %stackaddr$makeclosure53580, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48091, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist53311$ae480890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53582 = alloca %struct.ScmObj*, align 8
%argslist53311$ae480891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48091, %struct.ScmObj* %argslist53311$ae480890)
store volatile %struct.ScmObj* %argslist53311$ae480891, %struct.ScmObj** %stackaddr$prim53582, align 8
%stackaddr$prim53583 = alloca %struct.ScmObj*, align 8
%argslist53311$ae480892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48090, %struct.ScmObj* %argslist53311$ae480891)
store volatile %struct.ScmObj* %argslist53311$ae480892, %struct.ScmObj** %stackaddr$prim53583, align 8
%clofunc53584 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48089)
musttail call tailcc void %clofunc53584(%struct.ScmObj* %ae48089, %struct.ScmObj* %argslist53311$ae480892)
ret void
}

define tailcc void @proc_clo$ae48089(%struct.ScmObj* %env$ae48089,%struct.ScmObj* %current_45args52928) {
%stackaddr$env-ref53585 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48089, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53585
%stackaddr$env-ref53586 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48089, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53586
%stackaddr$env-ref53587 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48089, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53587
%stackaddr$env-ref53588 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48089, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53588
%stackaddr$env-ref53589 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48089, i64 4)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref53589
%stackaddr$env-ref53590 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48089, i64 5)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref53590
%stackaddr$prim53591 = alloca %struct.ScmObj*, align 8
%_95k47316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52928)
store volatile %struct.ScmObj* %_95k47316, %struct.ScmObj** %stackaddr$prim53591, align 8
%stackaddr$prim53592 = alloca %struct.ScmObj*, align 8
%current_45args52929 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52928)
store volatile %struct.ScmObj* %current_45args52929, %struct.ScmObj** %stackaddr$prim53592, align 8
%stackaddr$prim53593 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52929)
store volatile %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$prim53593, align 8
%stackaddr$makeclosure53594 = alloca %struct.ScmObj*, align 8
%fptrToInt53595 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48143 to i64
%ae48143 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53595)
store volatile %struct.ScmObj* %ae48143, %struct.ScmObj** %stackaddr$makeclosure53594, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48143, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48143, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48143, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48143, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48143, %struct.ScmObj* %_37last47112, i64 4)
%ae48144 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53596 = alloca %struct.ScmObj*, align 8
%fptrToInt53597 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48145 to i64
%ae48145 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53597)
store volatile %struct.ScmObj* %ae48145, %struct.ScmObj** %stackaddr$makeclosure53596, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48145, %struct.ScmObj* %_37take47082, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48145, %struct.ScmObj* %_37length47079, i64 1)
%argslist53297$ae481430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53598 = alloca %struct.ScmObj*, align 8
%argslist53297$ae481431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48145, %struct.ScmObj* %argslist53297$ae481430)
store volatile %struct.ScmObj* %argslist53297$ae481431, %struct.ScmObj** %stackaddr$prim53598, align 8
%stackaddr$prim53599 = alloca %struct.ScmObj*, align 8
%argslist53297$ae481432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48144, %struct.ScmObj* %argslist53297$ae481431)
store volatile %struct.ScmObj* %argslist53297$ae481432, %struct.ScmObj** %stackaddr$prim53599, align 8
%clofunc53600 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48143)
musttail call tailcc void %clofunc53600(%struct.ScmObj* %ae48143, %struct.ScmObj* %argslist53297$ae481432)
ret void
}

define tailcc void @proc_clo$ae48143(%struct.ScmObj* %env$ae48143,%struct.ScmObj* %current_45args52931) {
%stackaddr$env-ref53601 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48143, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53601
%stackaddr$env-ref53602 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48143, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53602
%stackaddr$env-ref53603 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48143, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53603
%stackaddr$env-ref53604 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48143, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53604
%stackaddr$env-ref53605 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48143, i64 4)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref53605
%stackaddr$prim53606 = alloca %struct.ScmObj*, align 8
%_95k47317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52931)
store volatile %struct.ScmObj* %_95k47317, %struct.ScmObj** %stackaddr$prim53606, align 8
%stackaddr$prim53607 = alloca %struct.ScmObj*, align 8
%current_45args52932 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52931)
store volatile %struct.ScmObj* %current_45args52932, %struct.ScmObj** %stackaddr$prim53607, align 8
%stackaddr$prim53608 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52932)
store volatile %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$prim53608, align 8
%stackaddr$makeclosure53609 = alloca %struct.ScmObj*, align 8
%fptrToInt53610 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48173 to i64
%ae48173 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53610)
store volatile %struct.ScmObj* %ae48173, %struct.ScmObj** %stackaddr$makeclosure53609, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48173, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48173, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48173, %struct.ScmObj* %_37drop_45right47109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48173, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48173, %struct.ScmObj* %_37last47112, i64 4)
%ae48174 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53611 = alloca %struct.ScmObj*, align 8
%fptrToInt53612 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48175 to i64
%ae48175 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53612)
store volatile %struct.ScmObj* %ae48175, %struct.ScmObj** %stackaddr$makeclosure53611, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48175, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48175, %struct.ScmObj* %_37map147086, i64 1)
%argslist53287$ae481730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53613 = alloca %struct.ScmObj*, align 8
%argslist53287$ae481731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48175, %struct.ScmObj* %argslist53287$ae481730)
store volatile %struct.ScmObj* %argslist53287$ae481731, %struct.ScmObj** %stackaddr$prim53613, align 8
%stackaddr$prim53614 = alloca %struct.ScmObj*, align 8
%argslist53287$ae481732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48174, %struct.ScmObj* %argslist53287$ae481731)
store volatile %struct.ScmObj* %argslist53287$ae481732, %struct.ScmObj** %stackaddr$prim53614, align 8
%clofunc53615 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48173)
musttail call tailcc void %clofunc53615(%struct.ScmObj* %ae48173, %struct.ScmObj* %argslist53287$ae481732)
ret void
}

define tailcc void @proc_clo$ae48173(%struct.ScmObj* %env$ae48173,%struct.ScmObj* %current_45args52934) {
%stackaddr$env-ref53616 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48173, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53616
%stackaddr$env-ref53617 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48173, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53617
%stackaddr$env-ref53618 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48173, i64 2)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref53618
%stackaddr$env-ref53619 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48173, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53619
%stackaddr$env-ref53620 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48173, i64 4)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref53620
%stackaddr$prim53621 = alloca %struct.ScmObj*, align 8
%_95k47318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52934)
store volatile %struct.ScmObj* %_95k47318, %struct.ScmObj** %stackaddr$prim53621, align 8
%stackaddr$prim53622 = alloca %struct.ScmObj*, align 8
%current_45args52935 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52934)
store volatile %struct.ScmObj* %current_45args52935, %struct.ScmObj** %stackaddr$prim53622, align 8
%stackaddr$prim53623 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52935)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim53623, align 8
%stackaddr$makeclosure53624 = alloca %struct.ScmObj*, align 8
%fptrToInt53625 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48557 to i64
%ae48557 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53625)
store volatile %struct.ScmObj* %ae48557, %struct.ScmObj** %stackaddr$makeclosure53624, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48557, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48557, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48557, %struct.ScmObj* %_37drop_45right47109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48557, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48557, %struct.ScmObj* %_37last47112, i64 4)
%argslist53227$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53626 = alloca %struct.ScmObj*, align 8
%argslist53227$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47238, %struct.ScmObj* %argslist53227$Ycmb470690)
store volatile %struct.ScmObj* %argslist53227$Ycmb470691, %struct.ScmObj** %stackaddr$prim53626, align 8
%stackaddr$prim53627 = alloca %struct.ScmObj*, align 8
%argslist53227$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48557, %struct.ScmObj* %argslist53227$Ycmb470691)
store volatile %struct.ScmObj* %argslist53227$Ycmb470692, %struct.ScmObj** %stackaddr$prim53627, align 8
%clofunc53628 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53628(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53227$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae48557(%struct.ScmObj* %env$ae48557,%struct.ScmObj* %current_45args52937) {
%stackaddr$env-ref53629 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48557, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53629
%stackaddr$env-ref53630 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48557, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53630
%stackaddr$env-ref53631 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48557, i64 2)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref53631
%stackaddr$env-ref53632 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48557, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53632
%stackaddr$env-ref53633 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48557, i64 4)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref53633
%stackaddr$prim53634 = alloca %struct.ScmObj*, align 8
%_95k47319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52937)
store volatile %struct.ScmObj* %_95k47319, %struct.ScmObj** %stackaddr$prim53634, align 8
%stackaddr$prim53635 = alloca %struct.ScmObj*, align 8
%current_45args52938 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52937)
store volatile %struct.ScmObj* %current_45args52938, %struct.ScmObj** %stackaddr$prim53635, align 8
%stackaddr$prim53636 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52938)
store volatile %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$prim53636, align 8
%stackaddr$makeclosure53637 = alloca %struct.ScmObj*, align 8
%fptrToInt53638 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48559 to i64
%ae48559 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53638)
store volatile %struct.ScmObj* %ae48559, %struct.ScmObj** %stackaddr$makeclosure53637, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48559, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48559, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48559, %struct.ScmObj* %_37drop_45right47109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48559, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48559, %struct.ScmObj* %_37last47112, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48559, %struct.ScmObj* %_37foldr47095, i64 5)
%ae48560 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53639 = alloca %struct.ScmObj*, align 8
%fptrToInt53640 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48561 to i64
%ae48561 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53640)
store volatile %struct.ScmObj* %ae48561, %struct.ScmObj** %stackaddr$makeclosure53639, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48561, %struct.ScmObj* %_37foldr147090, i64 0)
%argslist53226$ae485590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53641 = alloca %struct.ScmObj*, align 8
%argslist53226$ae485591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48561, %struct.ScmObj* %argslist53226$ae485590)
store volatile %struct.ScmObj* %argslist53226$ae485591, %struct.ScmObj** %stackaddr$prim53641, align 8
%stackaddr$prim53642 = alloca %struct.ScmObj*, align 8
%argslist53226$ae485592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48560, %struct.ScmObj* %argslist53226$ae485591)
store volatile %struct.ScmObj* %argslist53226$ae485592, %struct.ScmObj** %stackaddr$prim53642, align 8
%clofunc53643 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48559)
musttail call tailcc void %clofunc53643(%struct.ScmObj* %ae48559, %struct.ScmObj* %argslist53226$ae485592)
ret void
}

define tailcc void @proc_clo$ae48559(%struct.ScmObj* %env$ae48559,%struct.ScmObj* %current_45args52940) {
%stackaddr$env-ref53644 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48559, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53644
%stackaddr$env-ref53645 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48559, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53645
%stackaddr$env-ref53646 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48559, i64 2)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref53646
%stackaddr$env-ref53647 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48559, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53647
%stackaddr$env-ref53648 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48559, i64 4)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref53648
%stackaddr$env-ref53649 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48559, i64 5)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref53649
%stackaddr$prim53650 = alloca %struct.ScmObj*, align 8
%_95k47320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52940)
store volatile %struct.ScmObj* %_95k47320, %struct.ScmObj** %stackaddr$prim53650, align 8
%stackaddr$prim53651 = alloca %struct.ScmObj*, align 8
%current_45args52941 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52940)
store volatile %struct.ScmObj* %current_45args52941, %struct.ScmObj** %stackaddr$prim53651, align 8
%stackaddr$prim53652 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52941)
store volatile %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$prim53652, align 8
%stackaddr$makeclosure53653 = alloca %struct.ScmObj*, align 8
%fptrToInt53654 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48636 to i64
%ae48636 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53654)
store volatile %struct.ScmObj* %ae48636, %struct.ScmObj** %stackaddr$makeclosure53653, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48636, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48636, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48636, %struct.ScmObj* %_37map147121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48636, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48636, %struct.ScmObj* %_37foldr47095, i64 4)
%ae48637 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53655 = alloca %struct.ScmObj*, align 8
%fptrToInt53656 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48638 to i64
%ae48638 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53656)
store volatile %struct.ScmObj* %ae48638, %struct.ScmObj** %stackaddr$makeclosure53655, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48638, %struct.ScmObj* %_37drop_45right47109, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48638, %struct.ScmObj* %_37last47112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48638, %struct.ScmObj* %_37foldr47095, i64 2)
%argslist53207$ae486360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53657 = alloca %struct.ScmObj*, align 8
%argslist53207$ae486361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48638, %struct.ScmObj* %argslist53207$ae486360)
store volatile %struct.ScmObj* %argslist53207$ae486361, %struct.ScmObj** %stackaddr$prim53657, align 8
%stackaddr$prim53658 = alloca %struct.ScmObj*, align 8
%argslist53207$ae486362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48637, %struct.ScmObj* %argslist53207$ae486361)
store volatile %struct.ScmObj* %argslist53207$ae486362, %struct.ScmObj** %stackaddr$prim53658, align 8
%clofunc53659 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48636)
musttail call tailcc void %clofunc53659(%struct.ScmObj* %ae48636, %struct.ScmObj* %argslist53207$ae486362)
ret void
}

define tailcc void @proc_clo$ae48636(%struct.ScmObj* %env$ae48636,%struct.ScmObj* %current_45args52943) {
%stackaddr$env-ref53660 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48636, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53660
%stackaddr$env-ref53661 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48636, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53661
%stackaddr$env-ref53662 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48636, i64 2)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref53662
%stackaddr$env-ref53663 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48636, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53663
%stackaddr$env-ref53664 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48636, i64 4)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref53664
%stackaddr$prim53665 = alloca %struct.ScmObj*, align 8
%_95k47321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52943)
store volatile %struct.ScmObj* %_95k47321, %struct.ScmObj** %stackaddr$prim53665, align 8
%stackaddr$prim53666 = alloca %struct.ScmObj*, align 8
%current_45args52944 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52943)
store volatile %struct.ScmObj* %current_45args52944, %struct.ScmObj** %stackaddr$prim53666, align 8
%stackaddr$prim53667 = alloca %struct.ScmObj*, align 8
%_37map47116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52944)
store volatile %struct.ScmObj* %_37map47116, %struct.ScmObj** %stackaddr$prim53667, align 8
%stackaddr$makeclosure53668 = alloca %struct.ScmObj*, align 8
%fptrToInt53669 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48782 to i64
%ae48782 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53669)
store volatile %struct.ScmObj* %ae48782, %struct.ScmObj** %stackaddr$makeclosure53668, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48782, %struct.ScmObj* %_37foldl147074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48782, %struct.ScmObj* %Ycmb47069, i64 1)
%ae48783 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53670 = alloca %struct.ScmObj*, align 8
%fptrToInt53671 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48784 to i64
%ae48784 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53671)
store volatile %struct.ScmObj* %ae48784, %struct.ScmObj** %stackaddr$makeclosure53670, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48784, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48784, %struct.ScmObj* %_37map147121, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48784, %struct.ScmObj* %_37foldr47095, i64 2)
%argslist53190$ae487820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53672 = alloca %struct.ScmObj*, align 8
%argslist53190$ae487821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48784, %struct.ScmObj* %argslist53190$ae487820)
store volatile %struct.ScmObj* %argslist53190$ae487821, %struct.ScmObj** %stackaddr$prim53672, align 8
%stackaddr$prim53673 = alloca %struct.ScmObj*, align 8
%argslist53190$ae487822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48783, %struct.ScmObj* %argslist53190$ae487821)
store volatile %struct.ScmObj* %argslist53190$ae487822, %struct.ScmObj** %stackaddr$prim53673, align 8
%clofunc53674 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48782)
musttail call tailcc void %clofunc53674(%struct.ScmObj* %ae48782, %struct.ScmObj* %argslist53190$ae487822)
ret void
}

define tailcc void @proc_clo$ae48782(%struct.ScmObj* %env$ae48782,%struct.ScmObj* %current_45args52946) {
%stackaddr$env-ref53675 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48782, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53675
%stackaddr$env-ref53676 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48782, i64 1)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53676
%stackaddr$prim53677 = alloca %struct.ScmObj*, align 8
%_95k47322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52946)
store volatile %struct.ScmObj* %_95k47322, %struct.ScmObj** %stackaddr$prim53677, align 8
%stackaddr$prim53678 = alloca %struct.ScmObj*, align 8
%current_45args52947 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52946)
store volatile %struct.ScmObj* %current_45args52947, %struct.ScmObj** %stackaddr$prim53678, align 8
%stackaddr$prim53679 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52947)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim53679, align 8
%stackaddr$makeclosure53680 = alloca %struct.ScmObj*, align 8
%fptrToInt53681 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49174 to i64
%ae49174 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53681)
store volatile %struct.ScmObj* %ae49174, %struct.ScmObj** %stackaddr$makeclosure53680, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49174, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist53130$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53682 = alloca %struct.ScmObj*, align 8
%argslist53130$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47258, %struct.ScmObj* %argslist53130$Ycmb470690)
store volatile %struct.ScmObj* %argslist53130$Ycmb470691, %struct.ScmObj** %stackaddr$prim53682, align 8
%stackaddr$prim53683 = alloca %struct.ScmObj*, align 8
%argslist53130$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49174, %struct.ScmObj* %argslist53130$Ycmb470691)
store volatile %struct.ScmObj* %argslist53130$Ycmb470692, %struct.ScmObj** %stackaddr$prim53683, align 8
%clofunc53684 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53684(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53130$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae49174(%struct.ScmObj* %env$ae49174,%struct.ScmObj* %current_45args52949) {
%stackaddr$env-ref53685 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49174, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53685
%stackaddr$prim53686 = alloca %struct.ScmObj*, align 8
%_95k47323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52949)
store volatile %struct.ScmObj* %_95k47323, %struct.ScmObj** %stackaddr$prim53686, align 8
%stackaddr$prim53687 = alloca %struct.ScmObj*, align 8
%current_45args52950 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52949)
store volatile %struct.ScmObj* %current_45args52950, %struct.ScmObj** %stackaddr$prim53687, align 8
%stackaddr$prim53688 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52950)
store volatile %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$prim53688, align 8
%stackaddr$makeclosure53689 = alloca %struct.ScmObj*, align 8
%fptrToInt53690 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49176 to i64
%ae49176 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53690)
store volatile %struct.ScmObj* %ae49176, %struct.ScmObj** %stackaddr$makeclosure53689, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49176, %struct.ScmObj* %_37foldl147074, i64 0)
%ae49177 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53691 = alloca %struct.ScmObj*, align 8
%fptrToInt53692 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49178 to i64
%ae49178 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53692)
store volatile %struct.ScmObj* %ae49178, %struct.ScmObj** %stackaddr$makeclosure53691, align 8
%argslist53129$ae491760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53693 = alloca %struct.ScmObj*, align 8
%argslist53129$ae491761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49178, %struct.ScmObj* %argslist53129$ae491760)
store volatile %struct.ScmObj* %argslist53129$ae491761, %struct.ScmObj** %stackaddr$prim53693, align 8
%stackaddr$prim53694 = alloca %struct.ScmObj*, align 8
%argslist53129$ae491762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49177, %struct.ScmObj* %argslist53129$ae491761)
store volatile %struct.ScmObj* %argslist53129$ae491762, %struct.ScmObj** %stackaddr$prim53694, align 8
%clofunc53695 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49176)
musttail call tailcc void %clofunc53695(%struct.ScmObj* %ae49176, %struct.ScmObj* %argslist53129$ae491762)
ret void
}

define tailcc void @proc_clo$ae49176(%struct.ScmObj* %env$ae49176,%struct.ScmObj* %current_45args52952) {
%stackaddr$env-ref53696 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49176, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53696
%stackaddr$prim53697 = alloca %struct.ScmObj*, align 8
%_95k47324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52952)
store volatile %struct.ScmObj* %_95k47324, %struct.ScmObj** %stackaddr$prim53697, align 8
%stackaddr$prim53698 = alloca %struct.ScmObj*, align 8
%current_45args52953 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52952)
store volatile %struct.ScmObj* %current_45args52953, %struct.ScmObj** %stackaddr$prim53698, align 8
%stackaddr$prim53699 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52953)
store volatile %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$prim53699, align 8
%stackaddr$makeclosure53700 = alloca %struct.ScmObj*, align 8
%fptrToInt53701 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49200 to i64
%ae49200 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53701)
store volatile %struct.ScmObj* %ae49200, %struct.ScmObj** %stackaddr$makeclosure53700, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49200, %struct.ScmObj* %_37foldl147074, i64 0)
%ae49201 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53702 = alloca %struct.ScmObj*, align 8
%fptrToInt53703 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49202 to i64
%ae49202 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53703)
store volatile %struct.ScmObj* %ae49202, %struct.ScmObj** %stackaddr$makeclosure53702, align 8
%argslist53123$ae492000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53704 = alloca %struct.ScmObj*, align 8
%argslist53123$ae492001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49202, %struct.ScmObj* %argslist53123$ae492000)
store volatile %struct.ScmObj* %argslist53123$ae492001, %struct.ScmObj** %stackaddr$prim53704, align 8
%stackaddr$prim53705 = alloca %struct.ScmObj*, align 8
%argslist53123$ae492002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49201, %struct.ScmObj* %argslist53123$ae492001)
store volatile %struct.ScmObj* %argslist53123$ae492002, %struct.ScmObj** %stackaddr$prim53705, align 8
%clofunc53706 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49200)
musttail call tailcc void %clofunc53706(%struct.ScmObj* %ae49200, %struct.ScmObj* %argslist53123$ae492002)
ret void
}

define tailcc void @proc_clo$ae49200(%struct.ScmObj* %env$ae49200,%struct.ScmObj* %current_45args52955) {
%stackaddr$env-ref53707 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49200, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53707
%stackaddr$prim53708 = alloca %struct.ScmObj*, align 8
%_95k47325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52955)
store volatile %struct.ScmObj* %_95k47325, %struct.ScmObj** %stackaddr$prim53708, align 8
%stackaddr$prim53709 = alloca %struct.ScmObj*, align 8
%current_45args52956 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52955)
store volatile %struct.ScmObj* %current_45args52956, %struct.ScmObj** %stackaddr$prim53709, align 8
%stackaddr$prim53710 = alloca %struct.ScmObj*, align 8
%_37_62_6147166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52956)
store volatile %struct.ScmObj* %_37_62_6147166, %struct.ScmObj** %stackaddr$prim53710, align 8
%ae49224 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49225 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53711 = alloca %struct.ScmObj*, align 8
%_37append47162 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49224, %struct.ScmObj* %ae49225)
store volatile %struct.ScmObj* %_37append47162, %struct.ScmObj** %stackaddr$prim53711, align 8
%stackaddr$makeclosure53712 = alloca %struct.ScmObj*, align 8
%fptrToInt53713 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49226 to i64
%ae49226 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53713)
store volatile %struct.ScmObj* %ae49226, %struct.ScmObj** %stackaddr$makeclosure53712, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %_37foldl147074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %_37append47162, i64 1)
%ae49227 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53714 = alloca %struct.ScmObj*, align 8
%fptrToInt53715 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49228 to i64
%ae49228 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53715)
store volatile %struct.ScmObj* %ae49228, %struct.ScmObj** %stackaddr$makeclosure53714, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49228, %struct.ScmObj* %_37append47162, i64 0)
%argslist53117$ae492260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53716 = alloca %struct.ScmObj*, align 8
%argslist53117$ae492261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49228, %struct.ScmObj* %argslist53117$ae492260)
store volatile %struct.ScmObj* %argslist53117$ae492261, %struct.ScmObj** %stackaddr$prim53716, align 8
%stackaddr$prim53717 = alloca %struct.ScmObj*, align 8
%argslist53117$ae492262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49227, %struct.ScmObj* %argslist53117$ae492261)
store volatile %struct.ScmObj* %argslist53117$ae492262, %struct.ScmObj** %stackaddr$prim53717, align 8
%clofunc53718 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49226)
musttail call tailcc void %clofunc53718(%struct.ScmObj* %ae49226, %struct.ScmObj* %argslist53117$ae492262)
ret void
}

define tailcc void @proc_clo$ae49226(%struct.ScmObj* %env$ae49226,%struct.ScmObj* %current_45args52958) {
%stackaddr$env-ref53719 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53719
%stackaddr$env-ref53720 = alloca %struct.ScmObj*, align 8
%_37append47162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 1)
store %struct.ScmObj* %_37append47162, %struct.ScmObj** %stackaddr$env-ref53720
%stackaddr$prim53721 = alloca %struct.ScmObj*, align 8
%_95k47326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52958)
store volatile %struct.ScmObj* %_95k47326, %struct.ScmObj** %stackaddr$prim53721, align 8
%stackaddr$prim53722 = alloca %struct.ScmObj*, align 8
%current_45args52959 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52958)
store volatile %struct.ScmObj* %current_45args52959, %struct.ScmObj** %stackaddr$prim53722, align 8
%stackaddr$prim53723 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52959)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim53723, align 8
%ae49294 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53724 = alloca %struct.ScmObj*, align 8
%_95047163 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47162, %struct.ScmObj* %ae49294, %struct.ScmObj* %anf_45bind47266)
store volatile %struct.ScmObj* %_95047163, %struct.ScmObj** %stackaddr$prim53724, align 8
%ae49297 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53725 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47162, %struct.ScmObj* %ae49297)
store volatile %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$prim53725, align 8
%stackaddr$makeclosure53726 = alloca %struct.ScmObj*, align 8
%fptrToInt53727 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49298 to i64
%ae49298 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53727)
store volatile %struct.ScmObj* %ae49298, %struct.ScmObj** %stackaddr$makeclosure53726, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49298, %struct.ScmObj* %_37foldl147074, i64 0)
%ae49299 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53728 = alloca %struct.ScmObj*, align 8
%fptrToInt53729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49300 to i64
%ae49300 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53729)
store volatile %struct.ScmObj* %ae49300, %struct.ScmObj** %stackaddr$makeclosure53728, align 8
%argslist53106$ae492980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53730 = alloca %struct.ScmObj*, align 8
%argslist53106$ae492981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49300, %struct.ScmObj* %argslist53106$ae492980)
store volatile %struct.ScmObj* %argslist53106$ae492981, %struct.ScmObj** %stackaddr$prim53730, align 8
%stackaddr$prim53731 = alloca %struct.ScmObj*, align 8
%argslist53106$ae492982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49299, %struct.ScmObj* %argslist53106$ae492981)
store volatile %struct.ScmObj* %argslist53106$ae492982, %struct.ScmObj** %stackaddr$prim53731, align 8
%clofunc53732 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49298)
musttail call tailcc void %clofunc53732(%struct.ScmObj* %ae49298, %struct.ScmObj* %argslist53106$ae492982)
ret void
}

define tailcc void @proc_clo$ae49298(%struct.ScmObj* %env$ae49298,%struct.ScmObj* %current_45args52961) {
%stackaddr$env-ref53733 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49298, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53733
%stackaddr$prim53734 = alloca %struct.ScmObj*, align 8
%_95k47327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52961)
store volatile %struct.ScmObj* %_95k47327, %struct.ScmObj** %stackaddr$prim53734, align 8
%stackaddr$prim53735 = alloca %struct.ScmObj*, align 8
%current_45args52962 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52961)
store volatile %struct.ScmObj* %current_45args52962, %struct.ScmObj** %stackaddr$prim53735, align 8
%stackaddr$prim53736 = alloca %struct.ScmObj*, align 8
%_37list_6347154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52962)
store volatile %struct.ScmObj* %_37list_6347154, %struct.ScmObj** %stackaddr$prim53736, align 8
%stackaddr$makeclosure53737 = alloca %struct.ScmObj*, align 8
%fptrToInt53738 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49714 to i64
%ae49714 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53738)
store volatile %struct.ScmObj* %ae49714, %struct.ScmObj** %stackaddr$makeclosure53737, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49714, %struct.ScmObj* %_37foldl147074, i64 0)
%ae49715 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53739 = alloca %struct.ScmObj*, align 8
%fptrToInt53740 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49716 to i64
%ae49716 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53740)
store volatile %struct.ScmObj* %ae49716, %struct.ScmObj** %stackaddr$makeclosure53739, align 8
%argslist53081$ae497140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53741 = alloca %struct.ScmObj*, align 8
%argslist53081$ae497141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49716, %struct.ScmObj* %argslist53081$ae497140)
store volatile %struct.ScmObj* %argslist53081$ae497141, %struct.ScmObj** %stackaddr$prim53741, align 8
%stackaddr$prim53742 = alloca %struct.ScmObj*, align 8
%argslist53081$ae497142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49715, %struct.ScmObj* %argslist53081$ae497141)
store volatile %struct.ScmObj* %argslist53081$ae497142, %struct.ScmObj** %stackaddr$prim53742, align 8
%clofunc53743 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49714)
musttail call tailcc void %clofunc53743(%struct.ScmObj* %ae49714, %struct.ScmObj* %argslist53081$ae497142)
ret void
}

define tailcc void @proc_clo$ae49714(%struct.ScmObj* %env$ae49714,%struct.ScmObj* %current_45args52964) {
%stackaddr$env-ref53744 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49714, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53744
%stackaddr$prim53745 = alloca %struct.ScmObj*, align 8
%_95k47328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52964)
store volatile %struct.ScmObj* %_95k47328, %struct.ScmObj** %stackaddr$prim53745, align 8
%stackaddr$prim53746 = alloca %struct.ScmObj*, align 8
%current_45args52965 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52964)
store volatile %struct.ScmObj* %current_45args52965, %struct.ScmObj** %stackaddr$prim53746, align 8
%stackaddr$prim53747 = alloca %struct.ScmObj*, align 8
%_37drop47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52965)
store volatile %struct.ScmObj* %_37drop47145, %struct.ScmObj** %stackaddr$prim53747, align 8
%stackaddr$makeclosure53748 = alloca %struct.ScmObj*, align 8
%fptrToInt53749 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50250 to i64
%ae50250 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53749)
store volatile %struct.ScmObj* %ae50250, %struct.ScmObj** %stackaddr$makeclosure53748, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50250, %struct.ScmObj* %_37foldl147074, i64 0)
%ae50251 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53750 = alloca %struct.ScmObj*, align 8
%fptrToInt53751 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50252 to i64
%ae50252 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53751)
store volatile %struct.ScmObj* %ae50252, %struct.ScmObj** %stackaddr$makeclosure53750, align 8
%argslist53057$ae502500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53752 = alloca %struct.ScmObj*, align 8
%argslist53057$ae502501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50252, %struct.ScmObj* %argslist53057$ae502500)
store volatile %struct.ScmObj* %argslist53057$ae502501, %struct.ScmObj** %stackaddr$prim53752, align 8
%stackaddr$prim53753 = alloca %struct.ScmObj*, align 8
%argslist53057$ae502502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50251, %struct.ScmObj* %argslist53057$ae502501)
store volatile %struct.ScmObj* %argslist53057$ae502502, %struct.ScmObj** %stackaddr$prim53753, align 8
%clofunc53754 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50250)
musttail call tailcc void %clofunc53754(%struct.ScmObj* %ae50250, %struct.ScmObj* %argslist53057$ae502502)
ret void
}

define tailcc void @proc_clo$ae50250(%struct.ScmObj* %env$ae50250,%struct.ScmObj* %current_45args52967) {
%stackaddr$env-ref53755 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50250, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53755
%stackaddr$prim53756 = alloca %struct.ScmObj*, align 8
%_95k47329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52967)
store volatile %struct.ScmObj* %_95k47329, %struct.ScmObj** %stackaddr$prim53756, align 8
%stackaddr$prim53757 = alloca %struct.ScmObj*, align 8
%current_45args52968 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52967)
store volatile %struct.ScmObj* %current_45args52968, %struct.ScmObj** %stackaddr$prim53757, align 8
%stackaddr$prim53758 = alloca %struct.ScmObj*, align 8
%_37memv47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52968)
store volatile %struct.ScmObj* %_37memv47138, %struct.ScmObj** %stackaddr$prim53758, align 8
%stackaddr$makeclosure53759 = alloca %struct.ScmObj*, align 8
%fptrToInt53760 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50654 to i64
%ae50654 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53760)
store volatile %struct.ScmObj* %ae50654, %struct.ScmObj** %stackaddr$makeclosure53759, align 8
%ae50655 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53761 = alloca %struct.ScmObj*, align 8
%fptrToInt53762 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50656 to i64
%ae50656 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53762)
store volatile %struct.ScmObj* %ae50656, %struct.ScmObj** %stackaddr$makeclosure53761, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50656, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist53031$ae506540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53763 = alloca %struct.ScmObj*, align 8
%argslist53031$ae506541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50656, %struct.ScmObj* %argslist53031$ae506540)
store volatile %struct.ScmObj* %argslist53031$ae506541, %struct.ScmObj** %stackaddr$prim53763, align 8
%stackaddr$prim53764 = alloca %struct.ScmObj*, align 8
%argslist53031$ae506542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50655, %struct.ScmObj* %argslist53031$ae506541)
store volatile %struct.ScmObj* %argslist53031$ae506542, %struct.ScmObj** %stackaddr$prim53764, align 8
%clofunc53765 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50654)
musttail call tailcc void %clofunc53765(%struct.ScmObj* %ae50654, %struct.ScmObj* %argslist53031$ae506542)
ret void
}

define tailcc void @proc_clo$ae50654(%struct.ScmObj* %env$ae50654,%struct.ScmObj* %current_45args52970) {
%stackaddr$prim53766 = alloca %struct.ScmObj*, align 8
%_95k47330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52970)
store volatile %struct.ScmObj* %_95k47330, %struct.ScmObj** %stackaddr$prim53766, align 8
%stackaddr$prim53767 = alloca %struct.ScmObj*, align 8
%current_45args52971 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52970)
store volatile %struct.ScmObj* %current_45args52971, %struct.ScmObj** %stackaddr$prim53767, align 8
%stackaddr$prim53768 = alloca %struct.ScmObj*, align 8
%_37_4747134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52971)
store volatile %struct.ScmObj* %_37_4747134, %struct.ScmObj** %stackaddr$prim53768, align 8
%stackaddr$makeclosure53769 = alloca %struct.ScmObj*, align 8
%fptrToInt53770 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50752 to i64
%ae50752 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53770)
store volatile %struct.ScmObj* %ae50752, %struct.ScmObj** %stackaddr$makeclosure53769, align 8
%ae50753 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53771 = alloca %struct.ScmObj*, align 8
%fptrToInt53772 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50754 to i64
%ae50754 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53772)
store volatile %struct.ScmObj* %ae50754, %struct.ScmObj** %stackaddr$makeclosure53771, align 8
%argslist53018$ae507520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53773 = alloca %struct.ScmObj*, align 8
%argslist53018$ae507521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50754, %struct.ScmObj* %argslist53018$ae507520)
store volatile %struct.ScmObj* %argslist53018$ae507521, %struct.ScmObj** %stackaddr$prim53773, align 8
%stackaddr$prim53774 = alloca %struct.ScmObj*, align 8
%argslist53018$ae507522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50753, %struct.ScmObj* %argslist53018$ae507521)
store volatile %struct.ScmObj* %argslist53018$ae507522, %struct.ScmObj** %stackaddr$prim53774, align 8
%clofunc53775 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50752)
musttail call tailcc void %clofunc53775(%struct.ScmObj* %ae50752, %struct.ScmObj* %argslist53018$ae507522)
ret void
}

define tailcc void @proc_clo$ae50752(%struct.ScmObj* %env$ae50752,%struct.ScmObj* %current_45args52973) {
%stackaddr$prim53776 = alloca %struct.ScmObj*, align 8
%_95k47331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52973)
store volatile %struct.ScmObj* %_95k47331, %struct.ScmObj** %stackaddr$prim53776, align 8
%stackaddr$prim53777 = alloca %struct.ScmObj*, align 8
%current_45args52974 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52973)
store volatile %struct.ScmObj* %current_45args52974, %struct.ScmObj** %stackaddr$prim53777, align 8
%stackaddr$prim53778 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52974)
store volatile %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$prim53778, align 8
%stackaddr$makeclosure53779 = alloca %struct.ScmObj*, align 8
%fptrToInt53780 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50772 to i64
%ae50772 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53780)
store volatile %struct.ScmObj* %ae50772, %struct.ScmObj** %stackaddr$makeclosure53779, align 8
%ae50773 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53781 = alloca %struct.ScmObj*, align 8
%fptrToInt53782 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50774 to i64
%ae50774 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53782)
store volatile %struct.ScmObj* %ae50774, %struct.ScmObj** %stackaddr$makeclosure53781, align 8
%argslist53013$ae507720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53783 = alloca %struct.ScmObj*, align 8
%argslist53013$ae507721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50774, %struct.ScmObj* %argslist53013$ae507720)
store volatile %struct.ScmObj* %argslist53013$ae507721, %struct.ScmObj** %stackaddr$prim53783, align 8
%stackaddr$prim53784 = alloca %struct.ScmObj*, align 8
%argslist53013$ae507722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50773, %struct.ScmObj* %argslist53013$ae507721)
store volatile %struct.ScmObj* %argslist53013$ae507722, %struct.ScmObj** %stackaddr$prim53784, align 8
%clofunc53785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50772)
musttail call tailcc void %clofunc53785(%struct.ScmObj* %ae50772, %struct.ScmObj* %argslist53013$ae507722)
ret void
}

define tailcc void @proc_clo$ae50772(%struct.ScmObj* %env$ae50772,%struct.ScmObj* %current_45args52976) {
%stackaddr$prim53786 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52976)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim53786, align 8
%stackaddr$prim53787 = alloca %struct.ScmObj*, align 8
%current_45args52977 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52976)
store volatile %struct.ScmObj* %current_45args52977, %struct.ScmObj** %stackaddr$prim53787, align 8
%stackaddr$prim53788 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52977)
store volatile %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$prim53788, align 8
%stackaddr$makeclosure53789 = alloca %struct.ScmObj*, align 8
%fptrToInt53790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50794 to i64
%ae50794 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53790)
store volatile %struct.ScmObj* %ae50794, %struct.ScmObj** %stackaddr$makeclosure53789, align 8
%ae50795 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53791 = alloca %struct.ScmObj*, align 8
%fptrToInt53792 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50796 to i64
%ae50796 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53792)
store volatile %struct.ScmObj* %ae50796, %struct.ScmObj** %stackaddr$makeclosure53791, align 8
%argslist53008$ae507940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53793 = alloca %struct.ScmObj*, align 8
%argslist53008$ae507941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50796, %struct.ScmObj* %argslist53008$ae507940)
store volatile %struct.ScmObj* %argslist53008$ae507941, %struct.ScmObj** %stackaddr$prim53793, align 8
%stackaddr$prim53794 = alloca %struct.ScmObj*, align 8
%argslist53008$ae507942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50795, %struct.ScmObj* %argslist53008$ae507941)
store volatile %struct.ScmObj* %argslist53008$ae507942, %struct.ScmObj** %stackaddr$prim53794, align 8
%clofunc53795 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50794)
musttail call tailcc void %clofunc53795(%struct.ScmObj* %ae50794, %struct.ScmObj* %argslist53008$ae507942)
ret void
}

define tailcc void @proc_clo$ae50794(%struct.ScmObj* %env$ae50794,%struct.ScmObj* %current_45args52979) {
%stackaddr$prim53796 = alloca %struct.ScmObj*, align 8
%_95k47333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52979)
store volatile %struct.ScmObj* %_95k47333, %struct.ScmObj** %stackaddr$prim53796, align 8
%stackaddr$prim53797 = alloca %struct.ScmObj*, align 8
%current_45args52980 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52979)
store volatile %struct.ScmObj* %current_45args52980, %struct.ScmObj** %stackaddr$prim53797, align 8
%stackaddr$prim53798 = alloca %struct.ScmObj*, align 8
%_37third47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52980)
store volatile %struct.ScmObj* %_37third47128, %struct.ScmObj** %stackaddr$prim53798, align 8
%stackaddr$makeclosure53799 = alloca %struct.ScmObj*, align 8
%fptrToInt53800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50818 to i64
%ae50818 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53800)
store volatile %struct.ScmObj* %ae50818, %struct.ScmObj** %stackaddr$makeclosure53799, align 8
%ae50819 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53801 = alloca %struct.ScmObj*, align 8
%fptrToInt53802 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50820 to i64
%ae50820 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53802)
store volatile %struct.ScmObj* %ae50820, %struct.ScmObj** %stackaddr$makeclosure53801, align 8
%argslist53003$ae508180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53803 = alloca %struct.ScmObj*, align 8
%argslist53003$ae508181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50820, %struct.ScmObj* %argslist53003$ae508180)
store volatile %struct.ScmObj* %argslist53003$ae508181, %struct.ScmObj** %stackaddr$prim53803, align 8
%stackaddr$prim53804 = alloca %struct.ScmObj*, align 8
%argslist53003$ae508182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50819, %struct.ScmObj* %argslist53003$ae508181)
store volatile %struct.ScmObj* %argslist53003$ae508182, %struct.ScmObj** %stackaddr$prim53804, align 8
%clofunc53805 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50818)
musttail call tailcc void %clofunc53805(%struct.ScmObj* %ae50818, %struct.ScmObj* %argslist53003$ae508182)
ret void
}

define tailcc void @proc_clo$ae50818(%struct.ScmObj* %env$ae50818,%struct.ScmObj* %current_45args52982) {
%stackaddr$prim53806 = alloca %struct.ScmObj*, align 8
%_95k47334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52982)
store volatile %struct.ScmObj* %_95k47334, %struct.ScmObj** %stackaddr$prim53806, align 8
%stackaddr$prim53807 = alloca %struct.ScmObj*, align 8
%current_45args52983 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52982)
store volatile %struct.ScmObj* %current_45args52983, %struct.ScmObj** %stackaddr$prim53807, align 8
%stackaddr$prim53808 = alloca %struct.ScmObj*, align 8
%_37fourth47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52983)
store volatile %struct.ScmObj* %_37fourth47126, %struct.ScmObj** %stackaddr$prim53808, align 8
%stackaddr$makeclosure53809 = alloca %struct.ScmObj*, align 8
%fptrToInt53810 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50844 to i64
%ae50844 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53810)
store volatile %struct.ScmObj* %ae50844, %struct.ScmObj** %stackaddr$makeclosure53809, align 8
%ae50845 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53811 = alloca %struct.ScmObj*, align 8
%fptrToInt53812 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50846 to i64
%ae50846 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53812)
store volatile %struct.ScmObj* %ae50846, %struct.ScmObj** %stackaddr$makeclosure53811, align 8
%argslist52998$ae508440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53813 = alloca %struct.ScmObj*, align 8
%argslist52998$ae508441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50846, %struct.ScmObj* %argslist52998$ae508440)
store volatile %struct.ScmObj* %argslist52998$ae508441, %struct.ScmObj** %stackaddr$prim53813, align 8
%stackaddr$prim53814 = alloca %struct.ScmObj*, align 8
%argslist52998$ae508442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50845, %struct.ScmObj* %argslist52998$ae508441)
store volatile %struct.ScmObj* %argslist52998$ae508442, %struct.ScmObj** %stackaddr$prim53814, align 8
%clofunc53815 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50844)
musttail call tailcc void %clofunc53815(%struct.ScmObj* %ae50844, %struct.ScmObj* %argslist52998$ae508442)
ret void
}

define tailcc void @proc_clo$ae50844(%struct.ScmObj* %env$ae50844,%struct.ScmObj* %current_45args52985) {
%stackaddr$prim53816 = alloca %struct.ScmObj*, align 8
%_95k47335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52985)
store volatile %struct.ScmObj* %_95k47335, %struct.ScmObj** %stackaddr$prim53816, align 8
%stackaddr$prim53817 = alloca %struct.ScmObj*, align 8
%current_45args52986 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52985)
store volatile %struct.ScmObj* %current_45args52986, %struct.ScmObj** %stackaddr$prim53817, align 8
%stackaddr$prim53818 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52986)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim53818, align 8
%stackaddr$makeclosure53819 = alloca %struct.ScmObj*, align 8
%fptrToInt53820 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50885 to i64
%ae50885 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53820)
store volatile %struct.ScmObj* %ae50885, %struct.ScmObj** %stackaddr$makeclosure53819, align 8
%ae50886 = call %struct.ScmObj* @const_init_int(i64 2)
%ae50887 = call %struct.ScmObj* @const_init_int(i64 3)
%ae50888 = call %struct.ScmObj* @const_init_int(i64 4)
%ae50889 = call %struct.ScmObj* @const_init_int(i64 5)
%argslist52992$anf_45bind473020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53821 = alloca %struct.ScmObj*, align 8
%argslist52992$anf_45bind473021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50889, %struct.ScmObj* %argslist52992$anf_45bind473020)
store volatile %struct.ScmObj* %argslist52992$anf_45bind473021, %struct.ScmObj** %stackaddr$prim53821, align 8
%stackaddr$prim53822 = alloca %struct.ScmObj*, align 8
%argslist52992$anf_45bind473022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50888, %struct.ScmObj* %argslist52992$anf_45bind473021)
store volatile %struct.ScmObj* %argslist52992$anf_45bind473022, %struct.ScmObj** %stackaddr$prim53822, align 8
%stackaddr$prim53823 = alloca %struct.ScmObj*, align 8
%argslist52992$anf_45bind473023 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50887, %struct.ScmObj* %argslist52992$anf_45bind473022)
store volatile %struct.ScmObj* %argslist52992$anf_45bind473023, %struct.ScmObj** %stackaddr$prim53823, align 8
%stackaddr$prim53824 = alloca %struct.ScmObj*, align 8
%argslist52992$anf_45bind473024 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50886, %struct.ScmObj* %argslist52992$anf_45bind473023)
store volatile %struct.ScmObj* %argslist52992$anf_45bind473024, %struct.ScmObj** %stackaddr$prim53824, align 8
%stackaddr$prim53825 = alloca %struct.ScmObj*, align 8
%argslist52992$anf_45bind473025 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50885, %struct.ScmObj* %argslist52992$anf_45bind473024)
store volatile %struct.ScmObj* %argslist52992$anf_45bind473025, %struct.ScmObj** %stackaddr$prim53825, align 8
%clofunc53826 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47302)
musttail call tailcc void %clofunc53826(%struct.ScmObj* %anf_45bind47302, %struct.ScmObj* %argslist52992$anf_45bind473025)
ret void
}

define tailcc void @proc_clo$ae50885(%struct.ScmObj* %env$ae50885,%struct.ScmObj* %current_45args52988) {
%stackaddr$prim53827 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52988)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim53827, align 8
%stackaddr$prim53828 = alloca %struct.ScmObj*, align 8
%current_45args52989 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52988)
store volatile %struct.ScmObj* %current_45args52989, %struct.ScmObj** %stackaddr$prim53828, align 8
%stackaddr$prim53829 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52989)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim53829, align 8
%stackaddr$prim53830 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim53830, align 8
%argslist52991$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53831 = alloca %struct.ScmObj*, align 8
%argslist52991$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist52991$k0)
store volatile %struct.ScmObj* %argslist52991$k1, %struct.ScmObj** %stackaddr$prim53831, align 8
%clofunc53832 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc53832(%struct.ScmObj* %k, %struct.ScmObj* %argslist52991$k1)
ret void
}

define tailcc void @proc_clo$ae50846(%struct.ScmObj* %env$ae50846,%struct.ScmObj* %t470684718747336) {
%stackaddr$prim53833 = alloca %struct.ScmObj*, align 8
%k47337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t470684718747336)
store volatile %struct.ScmObj* %k47337, %struct.ScmObj** %stackaddr$prim53833, align 8
%stackaddr$prim53834 = alloca %struct.ScmObj*, align 8
%t4706847187 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t470684718747336)
store volatile %struct.ScmObj* %t4706847187, %struct.ScmObj** %stackaddr$prim53834, align 8
%stackaddr$prim53835 = alloca %struct.ScmObj*, align 8
%a47188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t4706847187)
store volatile %struct.ScmObj* %a47188, %struct.ScmObj** %stackaddr$prim53835, align 8
%stackaddr$prim53836 = alloca %struct.ScmObj*, align 8
%t4706847189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t4706847187)
store volatile %struct.ScmObj* %t4706847189, %struct.ScmObj** %stackaddr$prim53836, align 8
%stackaddr$makeclosure53837 = alloca %struct.ScmObj*, align 8
%fptrToInt53838 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50851 to i64
%ae50851 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53838)
store volatile %struct.ScmObj* %ae50851, %struct.ScmObj** %stackaddr$makeclosure53837, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50851, %struct.ScmObj* %k47337, i64 0)
%ae50852 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist52997$ae508510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53839 = alloca %struct.ScmObj*, align 8
%argslist52997$ae508511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %t4706847189, %struct.ScmObj* %argslist52997$ae508510)
store volatile %struct.ScmObj* %argslist52997$ae508511, %struct.ScmObj** %stackaddr$prim53839, align 8
%stackaddr$prim53840 = alloca %struct.ScmObj*, align 8
%argslist52997$ae508512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50852, %struct.ScmObj* %argslist52997$ae508511)
store volatile %struct.ScmObj* %argslist52997$ae508512, %struct.ScmObj** %stackaddr$prim53840, align 8
%clofunc53841 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50851)
musttail call tailcc void %clofunc53841(%struct.ScmObj* %ae50851, %struct.ScmObj* %argslist52997$ae508512)
ret void
}

define tailcc void @proc_clo$ae50851(%struct.ScmObj* %env$ae50851,%struct.ScmObj* %current_45args52993) {
%stackaddr$env-ref53842 = alloca %struct.ScmObj*, align 8
%k47337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50851, i64 0)
store %struct.ScmObj* %k47337, %struct.ScmObj** %stackaddr$env-ref53842
%stackaddr$prim53843 = alloca %struct.ScmObj*, align 8
%_95k47338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52993)
store volatile %struct.ScmObj* %_95k47338, %struct.ScmObj** %stackaddr$prim53843, align 8
%stackaddr$prim53844 = alloca %struct.ScmObj*, align 8
%current_45args52994 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52993)
store volatile %struct.ScmObj* %current_45args52994, %struct.ScmObj** %stackaddr$prim53844, align 8
%stackaddr$prim53845 = alloca %struct.ScmObj*, align 8
%b47190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52994)
store volatile %struct.ScmObj* %b47190, %struct.ScmObj** %stackaddr$prim53845, align 8
%stackaddr$prim53846 = alloca %struct.ScmObj*, align 8
%cpsprim47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %b47190)
store volatile %struct.ScmObj* %cpsprim47339, %struct.ScmObj** %stackaddr$prim53846, align 8
%ae50859 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist52996$k473370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53847 = alloca %struct.ScmObj*, align 8
%argslist52996$k473371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47339, %struct.ScmObj* %argslist52996$k473370)
store volatile %struct.ScmObj* %argslist52996$k473371, %struct.ScmObj** %stackaddr$prim53847, align 8
%stackaddr$prim53848 = alloca %struct.ScmObj*, align 8
%argslist52996$k473372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50859, %struct.ScmObj* %argslist52996$k473371)
store volatile %struct.ScmObj* %argslist52996$k473372, %struct.ScmObj** %stackaddr$prim53848, align 8
%clofunc53849 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47337)
musttail call tailcc void %clofunc53849(%struct.ScmObj* %k47337, %struct.ScmObj* %argslist52996$k473372)
ret void
}

define tailcc void @proc_clo$ae50820(%struct.ScmObj* %env$ae50820,%struct.ScmObj* %current_45args52999) {
%stackaddr$prim53850 = alloca %struct.ScmObj*, align 8
%k47340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52999)
store volatile %struct.ScmObj* %k47340, %struct.ScmObj** %stackaddr$prim53850, align 8
%stackaddr$prim53851 = alloca %struct.ScmObj*, align 8
%current_45args53000 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52999)
store volatile %struct.ScmObj* %current_45args53000, %struct.ScmObj** %stackaddr$prim53851, align 8
%stackaddr$prim53852 = alloca %struct.ScmObj*, align 8
%x47127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53000)
store volatile %struct.ScmObj* %x47127, %struct.ScmObj** %stackaddr$prim53852, align 8
%stackaddr$prim53853 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47127)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim53853, align 8
%stackaddr$prim53854 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47299)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim53854, align 8
%stackaddr$prim53855 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47300)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim53855, align 8
%stackaddr$prim53856 = alloca %struct.ScmObj*, align 8
%cpsprim47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47301)
store volatile %struct.ScmObj* %cpsprim47341, %struct.ScmObj** %stackaddr$prim53856, align 8
%ae50826 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53002$k473400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53857 = alloca %struct.ScmObj*, align 8
%argslist53002$k473401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47341, %struct.ScmObj* %argslist53002$k473400)
store volatile %struct.ScmObj* %argslist53002$k473401, %struct.ScmObj** %stackaddr$prim53857, align 8
%stackaddr$prim53858 = alloca %struct.ScmObj*, align 8
%argslist53002$k473402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50826, %struct.ScmObj* %argslist53002$k473401)
store volatile %struct.ScmObj* %argslist53002$k473402, %struct.ScmObj** %stackaddr$prim53858, align 8
%clofunc53859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47340)
musttail call tailcc void %clofunc53859(%struct.ScmObj* %k47340, %struct.ScmObj* %argslist53002$k473402)
ret void
}

define tailcc void @proc_clo$ae50796(%struct.ScmObj* %env$ae50796,%struct.ScmObj* %current_45args53004) {
%stackaddr$prim53860 = alloca %struct.ScmObj*, align 8
%k47342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53004)
store volatile %struct.ScmObj* %k47342, %struct.ScmObj** %stackaddr$prim53860, align 8
%stackaddr$prim53861 = alloca %struct.ScmObj*, align 8
%current_45args53005 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53004)
store volatile %struct.ScmObj* %current_45args53005, %struct.ScmObj** %stackaddr$prim53861, align 8
%stackaddr$prim53862 = alloca %struct.ScmObj*, align 8
%x47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53005)
store volatile %struct.ScmObj* %x47129, %struct.ScmObj** %stackaddr$prim53862, align 8
%stackaddr$prim53863 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47129)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim53863, align 8
%stackaddr$prim53864 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim53864, align 8
%stackaddr$prim53865 = alloca %struct.ScmObj*, align 8
%cpsprim47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47343, %struct.ScmObj** %stackaddr$prim53865, align 8
%ae50801 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53007$k473420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53866 = alloca %struct.ScmObj*, align 8
%argslist53007$k473421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47343, %struct.ScmObj* %argslist53007$k473420)
store volatile %struct.ScmObj* %argslist53007$k473421, %struct.ScmObj** %stackaddr$prim53866, align 8
%stackaddr$prim53867 = alloca %struct.ScmObj*, align 8
%argslist53007$k473422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50801, %struct.ScmObj* %argslist53007$k473421)
store volatile %struct.ScmObj* %argslist53007$k473422, %struct.ScmObj** %stackaddr$prim53867, align 8
%clofunc53868 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47342)
musttail call tailcc void %clofunc53868(%struct.ScmObj* %k47342, %struct.ScmObj* %argslist53007$k473422)
ret void
}

define tailcc void @proc_clo$ae50774(%struct.ScmObj* %env$ae50774,%struct.ScmObj* %current_45args53009) {
%stackaddr$prim53869 = alloca %struct.ScmObj*, align 8
%k47344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53009)
store volatile %struct.ScmObj* %k47344, %struct.ScmObj** %stackaddr$prim53869, align 8
%stackaddr$prim53870 = alloca %struct.ScmObj*, align 8
%current_45args53010 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53009)
store volatile %struct.ScmObj* %current_45args53010, %struct.ScmObj** %stackaddr$prim53870, align 8
%stackaddr$prim53871 = alloca %struct.ScmObj*, align 8
%x47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53010)
store volatile %struct.ScmObj* %x47131, %struct.ScmObj** %stackaddr$prim53871, align 8
%stackaddr$prim53872 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47131)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim53872, align 8
%stackaddr$prim53873 = alloca %struct.ScmObj*, align 8
%cpsprim47345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47296)
store volatile %struct.ScmObj* %cpsprim47345, %struct.ScmObj** %stackaddr$prim53873, align 8
%ae50778 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53012$k473440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53874 = alloca %struct.ScmObj*, align 8
%argslist53012$k473441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47345, %struct.ScmObj* %argslist53012$k473440)
store volatile %struct.ScmObj* %argslist53012$k473441, %struct.ScmObj** %stackaddr$prim53874, align 8
%stackaddr$prim53875 = alloca %struct.ScmObj*, align 8
%argslist53012$k473442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50778, %struct.ScmObj* %argslist53012$k473441)
store volatile %struct.ScmObj* %argslist53012$k473442, %struct.ScmObj** %stackaddr$prim53875, align 8
%clofunc53876 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47344)
musttail call tailcc void %clofunc53876(%struct.ScmObj* %k47344, %struct.ScmObj* %argslist53012$k473442)
ret void
}

define tailcc void @proc_clo$ae50754(%struct.ScmObj* %env$ae50754,%struct.ScmObj* %current_45args53014) {
%stackaddr$prim53877 = alloca %struct.ScmObj*, align 8
%k47346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53014)
store volatile %struct.ScmObj* %k47346, %struct.ScmObj** %stackaddr$prim53877, align 8
%stackaddr$prim53878 = alloca %struct.ScmObj*, align 8
%current_45args53015 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53014)
store volatile %struct.ScmObj* %current_45args53015, %struct.ScmObj** %stackaddr$prim53878, align 8
%stackaddr$prim53879 = alloca %struct.ScmObj*, align 8
%x47133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53015)
store volatile %struct.ScmObj* %x47133, %struct.ScmObj** %stackaddr$prim53879, align 8
%stackaddr$prim53880 = alloca %struct.ScmObj*, align 8
%cpsprim47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47133)
store volatile %struct.ScmObj* %cpsprim47347, %struct.ScmObj** %stackaddr$prim53880, align 8
%ae50757 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53017$k473460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53881 = alloca %struct.ScmObj*, align 8
%argslist53017$k473461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47347, %struct.ScmObj* %argslist53017$k473460)
store volatile %struct.ScmObj* %argslist53017$k473461, %struct.ScmObj** %stackaddr$prim53881, align 8
%stackaddr$prim53882 = alloca %struct.ScmObj*, align 8
%argslist53017$k473462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50757, %struct.ScmObj* %argslist53017$k473461)
store volatile %struct.ScmObj* %argslist53017$k473462, %struct.ScmObj** %stackaddr$prim53882, align 8
%clofunc53883 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47346)
musttail call tailcc void %clofunc53883(%struct.ScmObj* %k47346, %struct.ScmObj* %argslist53017$k473462)
ret void
}

define tailcc void @proc_clo$ae50656(%struct.ScmObj* %env$ae50656,%struct.ScmObj* %args4713547348) {
%stackaddr$env-ref53884 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50656, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53884
%stackaddr$prim53885 = alloca %struct.ScmObj*, align 8
%k47349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4713547348)
store volatile %struct.ScmObj* %k47349, %struct.ScmObj** %stackaddr$prim53885, align 8
%stackaddr$prim53886 = alloca %struct.ScmObj*, align 8
%args47135 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4713547348)
store volatile %struct.ScmObj* %args47135, %struct.ScmObj** %stackaddr$prim53886, align 8
%stackaddr$prim53887 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim53887, align 8
%truthy$cmp53888 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47290)
%cmp$cmp53888 = icmp eq i64 %truthy$cmp53888, 1
br i1 %cmp$cmp53888, label %truebranch$cmp53888, label %falsebranch$cmp53888
truebranch$cmp53888:
%ae50662 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50663 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53019$k473490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53889 = alloca %struct.ScmObj*, align 8
%argslist53019$k473491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50663, %struct.ScmObj* %argslist53019$k473490)
store volatile %struct.ScmObj* %argslist53019$k473491, %struct.ScmObj** %stackaddr$prim53889, align 8
%stackaddr$prim53890 = alloca %struct.ScmObj*, align 8
%argslist53019$k473492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50662, %struct.ScmObj* %argslist53019$k473491)
store volatile %struct.ScmObj* %argslist53019$k473492, %struct.ScmObj** %stackaddr$prim53890, align 8
%clofunc53891 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47349)
musttail call tailcc void %clofunc53891(%struct.ScmObj* %k47349, %struct.ScmObj* %argslist53019$k473492)
ret void
falsebranch$cmp53888:
%stackaddr$prim53892 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim53892, align 8
%stackaddr$prim53893 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47291)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim53893, align 8
%truthy$cmp53894 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47292)
%cmp$cmp53894 = icmp eq i64 %truthy$cmp53894, 1
br i1 %cmp$cmp53894, label %truebranch$cmp53894, label %falsebranch$cmp53894
truebranch$cmp53894:
%stackaddr$prim53895 = alloca %struct.ScmObj*, align 8
%cpsprim47350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %cpsprim47350, %struct.ScmObj** %stackaddr$prim53895, align 8
%ae50675 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53020$k473490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53896 = alloca %struct.ScmObj*, align 8
%argslist53020$k473491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47350, %struct.ScmObj* %argslist53020$k473490)
store volatile %struct.ScmObj* %argslist53020$k473491, %struct.ScmObj** %stackaddr$prim53896, align 8
%stackaddr$prim53897 = alloca %struct.ScmObj*, align 8
%argslist53020$k473492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50675, %struct.ScmObj* %argslist53020$k473491)
store volatile %struct.ScmObj* %argslist53020$k473492, %struct.ScmObj** %stackaddr$prim53897, align 8
%clofunc53898 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47349)
musttail call tailcc void %clofunc53898(%struct.ScmObj* %k47349, %struct.ScmObj* %argslist53020$k473492)
ret void
falsebranch$cmp53894:
%stackaddr$makeclosure53899 = alloca %struct.ScmObj*, align 8
%fptrToInt53900 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50680 to i64
%ae50680 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53900)
store volatile %struct.ScmObj* %ae50680, %struct.ScmObj** %stackaddr$makeclosure53899, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50680, %struct.ScmObj* %k47349, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50680, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50680, %struct.ScmObj* %args47135, i64 2)
%ae50681 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53901 = alloca %struct.ScmObj*, align 8
%fptrToInt53902 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50682 to i64
%ae50682 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53902)
store volatile %struct.ScmObj* %ae50682, %struct.ScmObj** %stackaddr$makeclosure53901, align 8
%argslist53030$ae506800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53903 = alloca %struct.ScmObj*, align 8
%argslist53030$ae506801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50682, %struct.ScmObj* %argslist53030$ae506800)
store volatile %struct.ScmObj* %argslist53030$ae506801, %struct.ScmObj** %stackaddr$prim53903, align 8
%stackaddr$prim53904 = alloca %struct.ScmObj*, align 8
%argslist53030$ae506802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50681, %struct.ScmObj* %argslist53030$ae506801)
store volatile %struct.ScmObj* %argslist53030$ae506802, %struct.ScmObj** %stackaddr$prim53904, align 8
%clofunc53905 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50680)
musttail call tailcc void %clofunc53905(%struct.ScmObj* %ae50680, %struct.ScmObj* %argslist53030$ae506802)
ret void
}

define tailcc void @proc_clo$ae50680(%struct.ScmObj* %env$ae50680,%struct.ScmObj* %current_45args53021) {
%stackaddr$env-ref53906 = alloca %struct.ScmObj*, align 8
%k47349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50680, i64 0)
store %struct.ScmObj* %k47349, %struct.ScmObj** %stackaddr$env-ref53906
%stackaddr$env-ref53907 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50680, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53907
%stackaddr$env-ref53908 = alloca %struct.ScmObj*, align 8
%args47135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50680, i64 2)
store %struct.ScmObj* %args47135, %struct.ScmObj** %stackaddr$env-ref53908
%stackaddr$prim53909 = alloca %struct.ScmObj*, align 8
%_95k47351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53021)
store volatile %struct.ScmObj* %_95k47351, %struct.ScmObj** %stackaddr$prim53909, align 8
%stackaddr$prim53910 = alloca %struct.ScmObj*, align 8
%current_45args53022 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53021)
store volatile %struct.ScmObj* %current_45args53022, %struct.ScmObj** %stackaddr$prim53910, align 8
%stackaddr$prim53911 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53022)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim53911, align 8
%stackaddr$prim53912 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim53912, align 8
%stackaddr$prim53913 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim53913, align 8
%argslist53024$_37foldl1470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53914 = alloca %struct.ScmObj*, align 8
%argslist53024$_37foldl1470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47295, %struct.ScmObj* %argslist53024$_37foldl1470740)
store volatile %struct.ScmObj* %argslist53024$_37foldl1470741, %struct.ScmObj** %stackaddr$prim53914, align 8
%stackaddr$prim53915 = alloca %struct.ScmObj*, align 8
%argslist53024$_37foldl1470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47294, %struct.ScmObj* %argslist53024$_37foldl1470741)
store volatile %struct.ScmObj* %argslist53024$_37foldl1470742, %struct.ScmObj** %stackaddr$prim53915, align 8
%stackaddr$prim53916 = alloca %struct.ScmObj*, align 8
%argslist53024$_37foldl1470743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47293, %struct.ScmObj* %argslist53024$_37foldl1470742)
store volatile %struct.ScmObj* %argslist53024$_37foldl1470743, %struct.ScmObj** %stackaddr$prim53916, align 8
%stackaddr$prim53917 = alloca %struct.ScmObj*, align 8
%argslist53024$_37foldl1470744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47349, %struct.ScmObj* %argslist53024$_37foldl1470743)
store volatile %struct.ScmObj* %argslist53024$_37foldl1470744, %struct.ScmObj** %stackaddr$prim53917, align 8
%clofunc53918 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147074)
musttail call tailcc void %clofunc53918(%struct.ScmObj* %_37foldl147074, %struct.ScmObj* %argslist53024$_37foldl1470744)
ret void
}

define tailcc void @proc_clo$ae50682(%struct.ScmObj* %env$ae50682,%struct.ScmObj* %current_45args53025) {
%stackaddr$prim53919 = alloca %struct.ScmObj*, align 8
%k47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53025)
store volatile %struct.ScmObj* %k47352, %struct.ScmObj** %stackaddr$prim53919, align 8
%stackaddr$prim53920 = alloca %struct.ScmObj*, align 8
%current_45args53026 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53025)
store volatile %struct.ScmObj* %current_45args53026, %struct.ScmObj** %stackaddr$prim53920, align 8
%stackaddr$prim53921 = alloca %struct.ScmObj*, align 8
%n47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53026)
store volatile %struct.ScmObj* %n47137, %struct.ScmObj** %stackaddr$prim53921, align 8
%stackaddr$prim53922 = alloca %struct.ScmObj*, align 8
%current_45args53027 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53026)
store volatile %struct.ScmObj* %current_45args53027, %struct.ScmObj** %stackaddr$prim53922, align 8
%stackaddr$prim53923 = alloca %struct.ScmObj*, align 8
%v47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53027)
store volatile %struct.ScmObj* %v47136, %struct.ScmObj** %stackaddr$prim53923, align 8
%stackaddr$prim53924 = alloca %struct.ScmObj*, align 8
%cpsprim47353 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47136, %struct.ScmObj* %n47137)
store volatile %struct.ScmObj* %cpsprim47353, %struct.ScmObj** %stackaddr$prim53924, align 8
%ae50686 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53029$k473520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53925 = alloca %struct.ScmObj*, align 8
%argslist53029$k473521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47353, %struct.ScmObj* %argslist53029$k473520)
store volatile %struct.ScmObj* %argslist53029$k473521, %struct.ScmObj** %stackaddr$prim53925, align 8
%stackaddr$prim53926 = alloca %struct.ScmObj*, align 8
%argslist53029$k473522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50686, %struct.ScmObj* %argslist53029$k473521)
store volatile %struct.ScmObj* %argslist53029$k473522, %struct.ScmObj** %stackaddr$prim53926, align 8
%clofunc53927 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47352)
musttail call tailcc void %clofunc53927(%struct.ScmObj* %k47352, %struct.ScmObj* %argslist53029$k473522)
ret void
}

define tailcc void @proc_clo$ae50252(%struct.ScmObj* %env$ae50252,%struct.ScmObj* %current_45args53032) {
%stackaddr$prim53928 = alloca %struct.ScmObj*, align 8
%k47354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53032)
store volatile %struct.ScmObj* %k47354, %struct.ScmObj** %stackaddr$prim53928, align 8
%stackaddr$prim53929 = alloca %struct.ScmObj*, align 8
%current_45args53033 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53032)
store volatile %struct.ScmObj* %current_45args53033, %struct.ScmObj** %stackaddr$prim53929, align 8
%stackaddr$prim53930 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53033)
store volatile %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$prim53930, align 8
%stackaddr$prim53931 = alloca %struct.ScmObj*, align 8
%current_45args53034 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53033)
store volatile %struct.ScmObj* %current_45args53034, %struct.ScmObj** %stackaddr$prim53931, align 8
%stackaddr$prim53932 = alloca %struct.ScmObj*, align 8
%lst47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53034)
store volatile %struct.ScmObj* %lst47139, %struct.ScmObj** %stackaddr$prim53932, align 8
%ae50253 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim53933 = alloca %struct.ScmObj*, align 8
%lst47141 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50253, %struct.ScmObj* %lst47139)
store volatile %struct.ScmObj* %lst47141, %struct.ScmObj** %stackaddr$prim53933, align 8
%stackaddr$makeclosure53934 = alloca %struct.ScmObj*, align 8
%fptrToInt53935 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50255 to i64
%ae50255 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53935)
store volatile %struct.ScmObj* %ae50255, %struct.ScmObj** %stackaddr$makeclosure53934, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50255, %struct.ScmObj* %lst47141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50255, %struct.ScmObj* %v47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50255, %struct.ScmObj* %k47354, i64 2)
%ae50256 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53936 = alloca %struct.ScmObj*, align 8
%fptrToInt53937 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50257 to i64
%ae50257 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53937)
store volatile %struct.ScmObj* %ae50257, %struct.ScmObj** %stackaddr$makeclosure53936, align 8
%argslist53056$ae502550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53938 = alloca %struct.ScmObj*, align 8
%argslist53056$ae502551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50257, %struct.ScmObj* %argslist53056$ae502550)
store volatile %struct.ScmObj* %argslist53056$ae502551, %struct.ScmObj** %stackaddr$prim53938, align 8
%stackaddr$prim53939 = alloca %struct.ScmObj*, align 8
%argslist53056$ae502552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50256, %struct.ScmObj* %argslist53056$ae502551)
store volatile %struct.ScmObj* %argslist53056$ae502552, %struct.ScmObj** %stackaddr$prim53939, align 8
%clofunc53940 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50255)
musttail call tailcc void %clofunc53940(%struct.ScmObj* %ae50255, %struct.ScmObj* %argslist53056$ae502552)
ret void
}

define tailcc void @proc_clo$ae50255(%struct.ScmObj* %env$ae50255,%struct.ScmObj* %current_45args53036) {
%stackaddr$env-ref53941 = alloca %struct.ScmObj*, align 8
%lst47141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50255, i64 0)
store %struct.ScmObj* %lst47141, %struct.ScmObj** %stackaddr$env-ref53941
%stackaddr$env-ref53942 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50255, i64 1)
store %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$env-ref53942
%stackaddr$env-ref53943 = alloca %struct.ScmObj*, align 8
%k47354 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50255, i64 2)
store %struct.ScmObj* %k47354, %struct.ScmObj** %stackaddr$env-ref53943
%stackaddr$prim53944 = alloca %struct.ScmObj*, align 8
%_95k47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53036)
store volatile %struct.ScmObj* %_95k47355, %struct.ScmObj** %stackaddr$prim53944, align 8
%stackaddr$prim53945 = alloca %struct.ScmObj*, align 8
%current_45args53037 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53036)
store volatile %struct.ScmObj* %current_45args53037, %struct.ScmObj** %stackaddr$prim53945, align 8
%stackaddr$prim53946 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53037)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim53946, align 8
%stackaddr$makeclosure53947 = alloca %struct.ScmObj*, align 8
%fptrToInt53948 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50271 to i64
%ae50271 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53948)
store volatile %struct.ScmObj* %ae50271, %struct.ScmObj** %stackaddr$makeclosure53947, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50271, %struct.ScmObj* %lst47141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50271, %struct.ScmObj* %v47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50271, %struct.ScmObj* %k47354, i64 2)
%stackaddr$makeclosure53949 = alloca %struct.ScmObj*, align 8
%fptrToInt53950 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50272 to i64
%ae50272 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53950)
store volatile %struct.ScmObj* %ae50272, %struct.ScmObj** %stackaddr$makeclosure53949, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50272, %struct.ScmObj* %lst47141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50272, %struct.ScmObj* %v47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50272, %struct.ScmObj* %k47354, i64 2)
%argslist53051$anf_45bind472820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53951 = alloca %struct.ScmObj*, align 8
%argslist53051$anf_45bind472821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50272, %struct.ScmObj* %argslist53051$anf_45bind472820)
store volatile %struct.ScmObj* %argslist53051$anf_45bind472821, %struct.ScmObj** %stackaddr$prim53951, align 8
%stackaddr$prim53952 = alloca %struct.ScmObj*, align 8
%argslist53051$anf_45bind472822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50271, %struct.ScmObj* %argslist53051$anf_45bind472821)
store volatile %struct.ScmObj* %argslist53051$anf_45bind472822, %struct.ScmObj** %stackaddr$prim53952, align 8
%clofunc53953 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47282)
musttail call tailcc void %clofunc53953(%struct.ScmObj* %anf_45bind47282, %struct.ScmObj* %argslist53051$anf_45bind472822)
ret void
}

define tailcc void @proc_clo$ae50271(%struct.ScmObj* %env$ae50271,%struct.ScmObj* %current_45args53039) {
%stackaddr$env-ref53954 = alloca %struct.ScmObj*, align 8
%lst47141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50271, i64 0)
store %struct.ScmObj* %lst47141, %struct.ScmObj** %stackaddr$env-ref53954
%stackaddr$env-ref53955 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50271, i64 1)
store %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$env-ref53955
%stackaddr$env-ref53956 = alloca %struct.ScmObj*, align 8
%k47354 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50271, i64 2)
store %struct.ScmObj* %k47354, %struct.ScmObj** %stackaddr$env-ref53956
%stackaddr$prim53957 = alloca %struct.ScmObj*, align 8
%_95k47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53039)
store volatile %struct.ScmObj* %_95k47356, %struct.ScmObj** %stackaddr$prim53957, align 8
%stackaddr$prim53958 = alloca %struct.ScmObj*, align 8
%current_45args53040 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53039)
store volatile %struct.ScmObj* %current_45args53040, %struct.ScmObj** %stackaddr$prim53958, align 8
%stackaddr$prim53959 = alloca %struct.ScmObj*, align 8
%cc47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53040)
store volatile %struct.ScmObj* %cc47142, %struct.ScmObj** %stackaddr$prim53959, align 8
%ae50380 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53960 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50380)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim53960, align 8
%stackaddr$prim53961 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim53961, align 8
%truthy$cmp53962 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47284)
%cmp$cmp53962 = icmp eq i64 %truthy$cmp53962, 1
br i1 %cmp$cmp53962, label %truebranch$cmp53962, label %falsebranch$cmp53962
truebranch$cmp53962:
%ae50384 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50385 = call %struct.ScmObj* @const_init_false()
%argslist53042$k473540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53963 = alloca %struct.ScmObj*, align 8
%argslist53042$k473541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50385, %struct.ScmObj* %argslist53042$k473540)
store volatile %struct.ScmObj* %argslist53042$k473541, %struct.ScmObj** %stackaddr$prim53963, align 8
%stackaddr$prim53964 = alloca %struct.ScmObj*, align 8
%argslist53042$k473542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50384, %struct.ScmObj* %argslist53042$k473541)
store volatile %struct.ScmObj* %argslist53042$k473542, %struct.ScmObj** %stackaddr$prim53964, align 8
%clofunc53965 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47354)
musttail call tailcc void %clofunc53965(%struct.ScmObj* %k47354, %struct.ScmObj* %argslist53042$k473542)
ret void
falsebranch$cmp53962:
%ae50393 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53966 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50393)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim53966, align 8
%stackaddr$prim53967 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47285)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim53967, align 8
%stackaddr$prim53968 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47286, %struct.ScmObj* %v47140)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim53968, align 8
%truthy$cmp53969 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47287)
%cmp$cmp53969 = icmp eq i64 %truthy$cmp53969, 1
br i1 %cmp$cmp53969, label %truebranch$cmp53969, label %falsebranch$cmp53969
truebranch$cmp53969:
%ae50399 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53970 = alloca %struct.ScmObj*, align 8
%cpsprim47357 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50399)
store volatile %struct.ScmObj* %cpsprim47357, %struct.ScmObj** %stackaddr$prim53970, align 8
%ae50401 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53043$k473540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53971 = alloca %struct.ScmObj*, align 8
%argslist53043$k473541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47357, %struct.ScmObj* %argslist53043$k473540)
store volatile %struct.ScmObj* %argslist53043$k473541, %struct.ScmObj** %stackaddr$prim53971, align 8
%stackaddr$prim53972 = alloca %struct.ScmObj*, align 8
%argslist53043$k473542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50401, %struct.ScmObj* %argslist53043$k473541)
store volatile %struct.ScmObj* %argslist53043$k473542, %struct.ScmObj** %stackaddr$prim53972, align 8
%clofunc53973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47354)
musttail call tailcc void %clofunc53973(%struct.ScmObj* %k47354, %struct.ScmObj* %argslist53043$k473542)
ret void
falsebranch$cmp53969:
%ae50412 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53974 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50412)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim53974, align 8
%stackaddr$prim53975 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim53975, align 8
%ae50415 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53976 = alloca %struct.ScmObj*, align 8
%_95047144 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50415, %struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %_95047144, %struct.ScmObj** %stackaddr$prim53976, align 8
%argslist53044$cc471420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53977 = alloca %struct.ScmObj*, align 8
%argslist53044$cc471421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47142, %struct.ScmObj* %argslist53044$cc471420)
store volatile %struct.ScmObj* %argslist53044$cc471421, %struct.ScmObj** %stackaddr$prim53977, align 8
%stackaddr$prim53978 = alloca %struct.ScmObj*, align 8
%argslist53044$cc471422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47354, %struct.ScmObj* %argslist53044$cc471421)
store volatile %struct.ScmObj* %argslist53044$cc471422, %struct.ScmObj** %stackaddr$prim53978, align 8
%clofunc53979 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47142)
musttail call tailcc void %clofunc53979(%struct.ScmObj* %cc47142, %struct.ScmObj* %argslist53044$cc471422)
ret void
}

define tailcc void @proc_clo$ae50272(%struct.ScmObj* %env$ae50272,%struct.ScmObj* %current_45args53045) {
%stackaddr$env-ref53980 = alloca %struct.ScmObj*, align 8
%lst47141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50272, i64 0)
store %struct.ScmObj* %lst47141, %struct.ScmObj** %stackaddr$env-ref53980
%stackaddr$env-ref53981 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50272, i64 1)
store %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$env-ref53981
%stackaddr$env-ref53982 = alloca %struct.ScmObj*, align 8
%k47354 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50272, i64 2)
store %struct.ScmObj* %k47354, %struct.ScmObj** %stackaddr$env-ref53982
%stackaddr$prim53983 = alloca %struct.ScmObj*, align 8
%_95k47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53045)
store volatile %struct.ScmObj* %_95k47356, %struct.ScmObj** %stackaddr$prim53983, align 8
%stackaddr$prim53984 = alloca %struct.ScmObj*, align 8
%current_45args53046 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53045)
store volatile %struct.ScmObj* %current_45args53046, %struct.ScmObj** %stackaddr$prim53984, align 8
%stackaddr$prim53985 = alloca %struct.ScmObj*, align 8
%cc47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53046)
store volatile %struct.ScmObj* %cc47142, %struct.ScmObj** %stackaddr$prim53985, align 8
%ae50274 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53986 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50274)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim53986, align 8
%stackaddr$prim53987 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim53987, align 8
%truthy$cmp53988 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47284)
%cmp$cmp53988 = icmp eq i64 %truthy$cmp53988, 1
br i1 %cmp$cmp53988, label %truebranch$cmp53988, label %falsebranch$cmp53988
truebranch$cmp53988:
%ae50278 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50279 = call %struct.ScmObj* @const_init_false()
%argslist53048$k473540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53989 = alloca %struct.ScmObj*, align 8
%argslist53048$k473541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50279, %struct.ScmObj* %argslist53048$k473540)
store volatile %struct.ScmObj* %argslist53048$k473541, %struct.ScmObj** %stackaddr$prim53989, align 8
%stackaddr$prim53990 = alloca %struct.ScmObj*, align 8
%argslist53048$k473542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50278, %struct.ScmObj* %argslist53048$k473541)
store volatile %struct.ScmObj* %argslist53048$k473542, %struct.ScmObj** %stackaddr$prim53990, align 8
%clofunc53991 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47354)
musttail call tailcc void %clofunc53991(%struct.ScmObj* %k47354, %struct.ScmObj* %argslist53048$k473542)
ret void
falsebranch$cmp53988:
%ae50287 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53992 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50287)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim53992, align 8
%stackaddr$prim53993 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47285)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim53993, align 8
%stackaddr$prim53994 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47286, %struct.ScmObj* %v47140)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim53994, align 8
%truthy$cmp53995 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47287)
%cmp$cmp53995 = icmp eq i64 %truthy$cmp53995, 1
br i1 %cmp$cmp53995, label %truebranch$cmp53995, label %falsebranch$cmp53995
truebranch$cmp53995:
%ae50293 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53996 = alloca %struct.ScmObj*, align 8
%cpsprim47357 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50293)
store volatile %struct.ScmObj* %cpsprim47357, %struct.ScmObj** %stackaddr$prim53996, align 8
%ae50295 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53049$k473540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53997 = alloca %struct.ScmObj*, align 8
%argslist53049$k473541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47357, %struct.ScmObj* %argslist53049$k473540)
store volatile %struct.ScmObj* %argslist53049$k473541, %struct.ScmObj** %stackaddr$prim53997, align 8
%stackaddr$prim53998 = alloca %struct.ScmObj*, align 8
%argslist53049$k473542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50295, %struct.ScmObj* %argslist53049$k473541)
store volatile %struct.ScmObj* %argslist53049$k473542, %struct.ScmObj** %stackaddr$prim53998, align 8
%clofunc53999 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47354)
musttail call tailcc void %clofunc53999(%struct.ScmObj* %k47354, %struct.ScmObj* %argslist53049$k473542)
ret void
falsebranch$cmp53995:
%ae50306 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54000 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50306)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim54000, align 8
%stackaddr$prim54001 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim54001, align 8
%ae50309 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54002 = alloca %struct.ScmObj*, align 8
%_95047144 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50309, %struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %_95047144, %struct.ScmObj** %stackaddr$prim54002, align 8
%argslist53050$cc471420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54003 = alloca %struct.ScmObj*, align 8
%argslist53050$cc471421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47142, %struct.ScmObj* %argslist53050$cc471420)
store volatile %struct.ScmObj* %argslist53050$cc471421, %struct.ScmObj** %stackaddr$prim54003, align 8
%stackaddr$prim54004 = alloca %struct.ScmObj*, align 8
%argslist53050$cc471422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47354, %struct.ScmObj* %argslist53050$cc471421)
store volatile %struct.ScmObj* %argslist53050$cc471422, %struct.ScmObj** %stackaddr$prim54004, align 8
%clofunc54005 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47142)
musttail call tailcc void %clofunc54005(%struct.ScmObj* %cc47142, %struct.ScmObj* %argslist53050$cc471422)
ret void
}

define tailcc void @proc_clo$ae50257(%struct.ScmObj* %env$ae50257,%struct.ScmObj* %current_45args53052) {
%stackaddr$prim54006 = alloca %struct.ScmObj*, align 8
%k47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53052)
store volatile %struct.ScmObj* %k47358, %struct.ScmObj** %stackaddr$prim54006, align 8
%stackaddr$prim54007 = alloca %struct.ScmObj*, align 8
%current_45args53053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53052)
store volatile %struct.ScmObj* %current_45args53053, %struct.ScmObj** %stackaddr$prim54007, align 8
%stackaddr$prim54008 = alloca %struct.ScmObj*, align 8
%u47143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53053)
store volatile %struct.ScmObj* %u47143, %struct.ScmObj** %stackaddr$prim54008, align 8
%argslist53055$u471430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54009 = alloca %struct.ScmObj*, align 8
%argslist53055$u471431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47143, %struct.ScmObj* %argslist53055$u471430)
store volatile %struct.ScmObj* %argslist53055$u471431, %struct.ScmObj** %stackaddr$prim54009, align 8
%stackaddr$prim54010 = alloca %struct.ScmObj*, align 8
%argslist53055$u471432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47358, %struct.ScmObj* %argslist53055$u471431)
store volatile %struct.ScmObj* %argslist53055$u471432, %struct.ScmObj** %stackaddr$prim54010, align 8
%clofunc54011 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47143)
musttail call tailcc void %clofunc54011(%struct.ScmObj* %u47143, %struct.ScmObj* %argslist53055$u471432)
ret void
}

define tailcc void @proc_clo$ae49716(%struct.ScmObj* %env$ae49716,%struct.ScmObj* %current_45args53058) {
%stackaddr$prim54012 = alloca %struct.ScmObj*, align 8
%k47359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53058)
store volatile %struct.ScmObj* %k47359, %struct.ScmObj** %stackaddr$prim54012, align 8
%stackaddr$prim54013 = alloca %struct.ScmObj*, align 8
%current_45args53059 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53058)
store volatile %struct.ScmObj* %current_45args53059, %struct.ScmObj** %stackaddr$prim54013, align 8
%stackaddr$prim54014 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53059)
store volatile %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$prim54014, align 8
%stackaddr$prim54015 = alloca %struct.ScmObj*, align 8
%current_45args53060 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53059)
store volatile %struct.ScmObj* %current_45args53060, %struct.ScmObj** %stackaddr$prim54015, align 8
%stackaddr$prim54016 = alloca %struct.ScmObj*, align 8
%n47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53060)
store volatile %struct.ScmObj* %n47146, %struct.ScmObj** %stackaddr$prim54016, align 8
%ae49717 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54017 = alloca %struct.ScmObj*, align 8
%n47149 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49717, %struct.ScmObj* %n47146)
store volatile %struct.ScmObj* %n47149, %struct.ScmObj** %stackaddr$prim54017, align 8
%ae49719 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54018 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49719, %struct.ScmObj* %lst47147)
store volatile %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$prim54018, align 8
%stackaddr$makeclosure54019 = alloca %struct.ScmObj*, align 8
%fptrToInt54020 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49721 to i64
%ae49721 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54020)
store volatile %struct.ScmObj* %ae49721, %struct.ScmObj** %stackaddr$makeclosure54019, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49721, %struct.ScmObj* %k47359, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49721, %struct.ScmObj* %n47149, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49721, %struct.ScmObj* %lst47148, i64 2)
%ae49722 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54021 = alloca %struct.ScmObj*, align 8
%fptrToInt54022 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49723 to i64
%ae49723 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54022)
store volatile %struct.ScmObj* %ae49723, %struct.ScmObj** %stackaddr$makeclosure54021, align 8
%argslist53080$ae497210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54023 = alloca %struct.ScmObj*, align 8
%argslist53080$ae497211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49723, %struct.ScmObj* %argslist53080$ae497210)
store volatile %struct.ScmObj* %argslist53080$ae497211, %struct.ScmObj** %stackaddr$prim54023, align 8
%stackaddr$prim54024 = alloca %struct.ScmObj*, align 8
%argslist53080$ae497212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49722, %struct.ScmObj* %argslist53080$ae497211)
store volatile %struct.ScmObj* %argslist53080$ae497212, %struct.ScmObj** %stackaddr$prim54024, align 8
%clofunc54025 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49721)
musttail call tailcc void %clofunc54025(%struct.ScmObj* %ae49721, %struct.ScmObj* %argslist53080$ae497212)
ret void
}

define tailcc void @proc_clo$ae49721(%struct.ScmObj* %env$ae49721,%struct.ScmObj* %current_45args53062) {
%stackaddr$env-ref54026 = alloca %struct.ScmObj*, align 8
%k47359 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49721, i64 0)
store %struct.ScmObj* %k47359, %struct.ScmObj** %stackaddr$env-ref54026
%stackaddr$env-ref54027 = alloca %struct.ScmObj*, align 8
%n47149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49721, i64 1)
store %struct.ScmObj* %n47149, %struct.ScmObj** %stackaddr$env-ref54027
%stackaddr$env-ref54028 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49721, i64 2)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref54028
%stackaddr$prim54029 = alloca %struct.ScmObj*, align 8
%_95k47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53062)
store volatile %struct.ScmObj* %_95k47360, %struct.ScmObj** %stackaddr$prim54029, align 8
%stackaddr$prim54030 = alloca %struct.ScmObj*, align 8
%current_45args53063 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53062)
store volatile %struct.ScmObj* %current_45args53063, %struct.ScmObj** %stackaddr$prim54030, align 8
%stackaddr$prim54031 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53063)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim54031, align 8
%stackaddr$makeclosure54032 = alloca %struct.ScmObj*, align 8
%fptrToInt54033 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49737 to i64
%ae49737 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54033)
store volatile %struct.ScmObj* %ae49737, %struct.ScmObj** %stackaddr$makeclosure54032, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49737, %struct.ScmObj* %k47359, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49737, %struct.ScmObj* %n47149, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49737, %struct.ScmObj* %lst47148, i64 2)
%stackaddr$makeclosure54034 = alloca %struct.ScmObj*, align 8
%fptrToInt54035 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49738 to i64
%ae49738 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54035)
store volatile %struct.ScmObj* %ae49738, %struct.ScmObj** %stackaddr$makeclosure54034, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49738, %struct.ScmObj* %k47359, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49738, %struct.ScmObj* %n47149, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49738, %struct.ScmObj* %lst47148, i64 2)
%argslist53075$anf_45bind472750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54036 = alloca %struct.ScmObj*, align 8
%argslist53075$anf_45bind472751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49738, %struct.ScmObj* %argslist53075$anf_45bind472750)
store volatile %struct.ScmObj* %argslist53075$anf_45bind472751, %struct.ScmObj** %stackaddr$prim54036, align 8
%stackaddr$prim54037 = alloca %struct.ScmObj*, align 8
%argslist53075$anf_45bind472752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49737, %struct.ScmObj* %argslist53075$anf_45bind472751)
store volatile %struct.ScmObj* %argslist53075$anf_45bind472752, %struct.ScmObj** %stackaddr$prim54037, align 8
%clofunc54038 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47275)
musttail call tailcc void %clofunc54038(%struct.ScmObj* %anf_45bind47275, %struct.ScmObj* %argslist53075$anf_45bind472752)
ret void
}

define tailcc void @proc_clo$ae49737(%struct.ScmObj* %env$ae49737,%struct.ScmObj* %current_45args53065) {
%stackaddr$env-ref54039 = alloca %struct.ScmObj*, align 8
%k47359 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49737, i64 0)
store %struct.ScmObj* %k47359, %struct.ScmObj** %stackaddr$env-ref54039
%stackaddr$env-ref54040 = alloca %struct.ScmObj*, align 8
%n47149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49737, i64 1)
store %struct.ScmObj* %n47149, %struct.ScmObj** %stackaddr$env-ref54040
%stackaddr$env-ref54041 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49737, i64 2)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref54041
%stackaddr$prim54042 = alloca %struct.ScmObj*, align 8
%_95k47361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53065)
store volatile %struct.ScmObj* %_95k47361, %struct.ScmObj** %stackaddr$prim54042, align 8
%stackaddr$prim54043 = alloca %struct.ScmObj*, align 8
%current_45args53066 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53065)
store volatile %struct.ScmObj* %current_45args53066, %struct.ScmObj** %stackaddr$prim54043, align 8
%stackaddr$prim54044 = alloca %struct.ScmObj*, align 8
%cc47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53066)
store volatile %struct.ScmObj* %cc47150, %struct.ScmObj** %stackaddr$prim54044, align 8
%ae49880 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54045 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49880)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim54045, align 8
%ae49881 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54046 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49881, %struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim54046, align 8
%truthy$cmp54047 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47277)
%cmp$cmp54047 = icmp eq i64 %truthy$cmp54047, 1
br i1 %cmp$cmp54047, label %truebranch$cmp54047, label %falsebranch$cmp54047
truebranch$cmp54047:
%ae49885 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54048 = alloca %struct.ScmObj*, align 8
%cpsprim47362 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49885)
store volatile %struct.ScmObj* %cpsprim47362, %struct.ScmObj** %stackaddr$prim54048, align 8
%ae49887 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53068$k473590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54049 = alloca %struct.ScmObj*, align 8
%argslist53068$k473591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47362, %struct.ScmObj* %argslist53068$k473590)
store volatile %struct.ScmObj* %argslist53068$k473591, %struct.ScmObj** %stackaddr$prim54049, align 8
%stackaddr$prim54050 = alloca %struct.ScmObj*, align 8
%argslist53068$k473592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49887, %struct.ScmObj* %argslist53068$k473591)
store volatile %struct.ScmObj* %argslist53068$k473592, %struct.ScmObj** %stackaddr$prim54050, align 8
%clofunc54051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47359)
musttail call tailcc void %clofunc54051(%struct.ScmObj* %k47359, %struct.ScmObj* %argslist53068$k473592)
ret void
falsebranch$cmp54047:
%ae49898 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54052 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49898)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim54052, align 8
%stackaddr$prim54053 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47278)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim54053, align 8
%ae49901 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54054 = alloca %struct.ScmObj*, align 8
%_95047153 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49901, %struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %_95047153, %struct.ScmObj** %stackaddr$prim54054, align 8
%ae49904 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54055 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49904)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim54055, align 8
%ae49906 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54056 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47280, %struct.ScmObj* %ae49906)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim54056, align 8
%ae49908 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54057 = alloca %struct.ScmObj*, align 8
%_95147152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49908, %struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %_95147152, %struct.ScmObj** %stackaddr$prim54057, align 8
%argslist53069$cc471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54058 = alloca %struct.ScmObj*, align 8
%argslist53069$cc471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47150, %struct.ScmObj* %argslist53069$cc471500)
store volatile %struct.ScmObj* %argslist53069$cc471501, %struct.ScmObj** %stackaddr$prim54058, align 8
%stackaddr$prim54059 = alloca %struct.ScmObj*, align 8
%argslist53069$cc471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47359, %struct.ScmObj* %argslist53069$cc471501)
store volatile %struct.ScmObj* %argslist53069$cc471502, %struct.ScmObj** %stackaddr$prim54059, align 8
%clofunc54060 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47150)
musttail call tailcc void %clofunc54060(%struct.ScmObj* %cc47150, %struct.ScmObj* %argslist53069$cc471502)
ret void
}

define tailcc void @proc_clo$ae49738(%struct.ScmObj* %env$ae49738,%struct.ScmObj* %current_45args53070) {
%stackaddr$env-ref54061 = alloca %struct.ScmObj*, align 8
%k47359 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49738, i64 0)
store %struct.ScmObj* %k47359, %struct.ScmObj** %stackaddr$env-ref54061
%stackaddr$env-ref54062 = alloca %struct.ScmObj*, align 8
%n47149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49738, i64 1)
store %struct.ScmObj* %n47149, %struct.ScmObj** %stackaddr$env-ref54062
%stackaddr$env-ref54063 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49738, i64 2)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref54063
%stackaddr$prim54064 = alloca %struct.ScmObj*, align 8
%_95k47361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53070)
store volatile %struct.ScmObj* %_95k47361, %struct.ScmObj** %stackaddr$prim54064, align 8
%stackaddr$prim54065 = alloca %struct.ScmObj*, align 8
%current_45args53071 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53070)
store volatile %struct.ScmObj* %current_45args53071, %struct.ScmObj** %stackaddr$prim54065, align 8
%stackaddr$prim54066 = alloca %struct.ScmObj*, align 8
%cc47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53071)
store volatile %struct.ScmObj* %cc47150, %struct.ScmObj** %stackaddr$prim54066, align 8
%ae49740 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54067 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49740)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim54067, align 8
%ae49741 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54068 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49741, %struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim54068, align 8
%truthy$cmp54069 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47277)
%cmp$cmp54069 = icmp eq i64 %truthy$cmp54069, 1
br i1 %cmp$cmp54069, label %truebranch$cmp54069, label %falsebranch$cmp54069
truebranch$cmp54069:
%ae49745 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54070 = alloca %struct.ScmObj*, align 8
%cpsprim47362 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49745)
store volatile %struct.ScmObj* %cpsprim47362, %struct.ScmObj** %stackaddr$prim54070, align 8
%ae49747 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53073$k473590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54071 = alloca %struct.ScmObj*, align 8
%argslist53073$k473591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47362, %struct.ScmObj* %argslist53073$k473590)
store volatile %struct.ScmObj* %argslist53073$k473591, %struct.ScmObj** %stackaddr$prim54071, align 8
%stackaddr$prim54072 = alloca %struct.ScmObj*, align 8
%argslist53073$k473592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49747, %struct.ScmObj* %argslist53073$k473591)
store volatile %struct.ScmObj* %argslist53073$k473592, %struct.ScmObj** %stackaddr$prim54072, align 8
%clofunc54073 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47359)
musttail call tailcc void %clofunc54073(%struct.ScmObj* %k47359, %struct.ScmObj* %argslist53073$k473592)
ret void
falsebranch$cmp54069:
%ae49758 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54074 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49758)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim54074, align 8
%stackaddr$prim54075 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47278)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim54075, align 8
%ae49761 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54076 = alloca %struct.ScmObj*, align 8
%_95047153 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49761, %struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %_95047153, %struct.ScmObj** %stackaddr$prim54076, align 8
%ae49764 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54077 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49764)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim54077, align 8
%ae49766 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54078 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47280, %struct.ScmObj* %ae49766)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim54078, align 8
%ae49768 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54079 = alloca %struct.ScmObj*, align 8
%_95147152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49768, %struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %_95147152, %struct.ScmObj** %stackaddr$prim54079, align 8
%argslist53074$cc471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54080 = alloca %struct.ScmObj*, align 8
%argslist53074$cc471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47150, %struct.ScmObj* %argslist53074$cc471500)
store volatile %struct.ScmObj* %argslist53074$cc471501, %struct.ScmObj** %stackaddr$prim54080, align 8
%stackaddr$prim54081 = alloca %struct.ScmObj*, align 8
%argslist53074$cc471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47359, %struct.ScmObj* %argslist53074$cc471501)
store volatile %struct.ScmObj* %argslist53074$cc471502, %struct.ScmObj** %stackaddr$prim54081, align 8
%clofunc54082 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47150)
musttail call tailcc void %clofunc54082(%struct.ScmObj* %cc47150, %struct.ScmObj* %argslist53074$cc471502)
ret void
}

define tailcc void @proc_clo$ae49723(%struct.ScmObj* %env$ae49723,%struct.ScmObj* %current_45args53076) {
%stackaddr$prim54083 = alloca %struct.ScmObj*, align 8
%k47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53076)
store volatile %struct.ScmObj* %k47363, %struct.ScmObj** %stackaddr$prim54083, align 8
%stackaddr$prim54084 = alloca %struct.ScmObj*, align 8
%current_45args53077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53076)
store volatile %struct.ScmObj* %current_45args53077, %struct.ScmObj** %stackaddr$prim54084, align 8
%stackaddr$prim54085 = alloca %struct.ScmObj*, align 8
%u47151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53077)
store volatile %struct.ScmObj* %u47151, %struct.ScmObj** %stackaddr$prim54085, align 8
%argslist53079$u471510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54086 = alloca %struct.ScmObj*, align 8
%argslist53079$u471511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47151, %struct.ScmObj* %argslist53079$u471510)
store volatile %struct.ScmObj* %argslist53079$u471511, %struct.ScmObj** %stackaddr$prim54086, align 8
%stackaddr$prim54087 = alloca %struct.ScmObj*, align 8
%argslist53079$u471512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47363, %struct.ScmObj* %argslist53079$u471511)
store volatile %struct.ScmObj* %argslist53079$u471512, %struct.ScmObj** %stackaddr$prim54087, align 8
%clofunc54088 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47151)
musttail call tailcc void %clofunc54088(%struct.ScmObj* %u47151, %struct.ScmObj* %argslist53079$u471512)
ret void
}

define tailcc void @proc_clo$ae49300(%struct.ScmObj* %env$ae49300,%struct.ScmObj* %current_45args53082) {
%stackaddr$prim54089 = alloca %struct.ScmObj*, align 8
%k47364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53082)
store volatile %struct.ScmObj* %k47364, %struct.ScmObj** %stackaddr$prim54089, align 8
%stackaddr$prim54090 = alloca %struct.ScmObj*, align 8
%current_45args53083 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53082)
store volatile %struct.ScmObj* %current_45args53083, %struct.ScmObj** %stackaddr$prim54090, align 8
%stackaddr$prim54091 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53083)
store volatile %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$prim54091, align 8
%ae49301 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54092 = alloca %struct.ScmObj*, align 8
%a47156 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49301, %struct.ScmObj* %a47155)
store volatile %struct.ScmObj* %a47156, %struct.ScmObj** %stackaddr$prim54092, align 8
%stackaddr$makeclosure54093 = alloca %struct.ScmObj*, align 8
%fptrToInt54094 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49303 to i64
%ae49303 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54094)
store volatile %struct.ScmObj* %ae49303, %struct.ScmObj** %stackaddr$makeclosure54093, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49303, %struct.ScmObj* %a47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49303, %struct.ScmObj* %k47364, i64 1)
%ae49304 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54095 = alloca %struct.ScmObj*, align 8
%fptrToInt54096 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49305 to i64
%ae49305 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54096)
store volatile %struct.ScmObj* %ae49305, %struct.ScmObj** %stackaddr$makeclosure54095, align 8
%argslist53105$ae493030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54097 = alloca %struct.ScmObj*, align 8
%argslist53105$ae493031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49305, %struct.ScmObj* %argslist53105$ae493030)
store volatile %struct.ScmObj* %argslist53105$ae493031, %struct.ScmObj** %stackaddr$prim54097, align 8
%stackaddr$prim54098 = alloca %struct.ScmObj*, align 8
%argslist53105$ae493032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49304, %struct.ScmObj* %argslist53105$ae493031)
store volatile %struct.ScmObj* %argslist53105$ae493032, %struct.ScmObj** %stackaddr$prim54098, align 8
%clofunc54099 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49303)
musttail call tailcc void %clofunc54099(%struct.ScmObj* %ae49303, %struct.ScmObj* %argslist53105$ae493032)
ret void
}

define tailcc void @proc_clo$ae49303(%struct.ScmObj* %env$ae49303,%struct.ScmObj* %current_45args53085) {
%stackaddr$env-ref54100 = alloca %struct.ScmObj*, align 8
%a47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49303, i64 0)
store %struct.ScmObj* %a47156, %struct.ScmObj** %stackaddr$env-ref54100
%stackaddr$env-ref54101 = alloca %struct.ScmObj*, align 8
%k47364 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49303, i64 1)
store %struct.ScmObj* %k47364, %struct.ScmObj** %stackaddr$env-ref54101
%stackaddr$prim54102 = alloca %struct.ScmObj*, align 8
%_95k47365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53085)
store volatile %struct.ScmObj* %_95k47365, %struct.ScmObj** %stackaddr$prim54102, align 8
%stackaddr$prim54103 = alloca %struct.ScmObj*, align 8
%current_45args53086 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53085)
store volatile %struct.ScmObj* %current_45args53086, %struct.ScmObj** %stackaddr$prim54103, align 8
%stackaddr$prim54104 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53086)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim54104, align 8
%stackaddr$makeclosure54105 = alloca %struct.ScmObj*, align 8
%fptrToInt54106 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49322 to i64
%ae49322 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54106)
store volatile %struct.ScmObj* %ae49322, %struct.ScmObj** %stackaddr$makeclosure54105, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49322, %struct.ScmObj* %a47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49322, %struct.ScmObj* %k47364, i64 1)
%stackaddr$makeclosure54107 = alloca %struct.ScmObj*, align 8
%fptrToInt54108 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49323 to i64
%ae49323 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54108)
store volatile %struct.ScmObj* %ae49323, %struct.ScmObj** %stackaddr$makeclosure54107, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49323, %struct.ScmObj* %a47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49323, %struct.ScmObj* %k47364, i64 1)
%argslist53100$anf_45bind472670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54109 = alloca %struct.ScmObj*, align 8
%argslist53100$anf_45bind472671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49323, %struct.ScmObj* %argslist53100$anf_45bind472670)
store volatile %struct.ScmObj* %argslist53100$anf_45bind472671, %struct.ScmObj** %stackaddr$prim54109, align 8
%stackaddr$prim54110 = alloca %struct.ScmObj*, align 8
%argslist53100$anf_45bind472672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49322, %struct.ScmObj* %argslist53100$anf_45bind472671)
store volatile %struct.ScmObj* %argslist53100$anf_45bind472672, %struct.ScmObj** %stackaddr$prim54110, align 8
%clofunc54111 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47267)
musttail call tailcc void %clofunc54111(%struct.ScmObj* %anf_45bind47267, %struct.ScmObj* %argslist53100$anf_45bind472672)
ret void
}

define tailcc void @proc_clo$ae49322(%struct.ScmObj* %env$ae49322,%struct.ScmObj* %current_45args53088) {
%stackaddr$env-ref54112 = alloca %struct.ScmObj*, align 8
%a47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49322, i64 0)
store %struct.ScmObj* %a47156, %struct.ScmObj** %stackaddr$env-ref54112
%stackaddr$env-ref54113 = alloca %struct.ScmObj*, align 8
%k47364 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49322, i64 1)
store %struct.ScmObj* %k47364, %struct.ScmObj** %stackaddr$env-ref54113
%stackaddr$prim54114 = alloca %struct.ScmObj*, align 8
%_95k47366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53088)
store volatile %struct.ScmObj* %_95k47366, %struct.ScmObj** %stackaddr$prim54114, align 8
%stackaddr$prim54115 = alloca %struct.ScmObj*, align 8
%current_45args53089 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53088)
store volatile %struct.ScmObj* %current_45args53089, %struct.ScmObj** %stackaddr$prim54115, align 8
%stackaddr$prim54116 = alloca %struct.ScmObj*, align 8
%cc47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53089)
store volatile %struct.ScmObj* %cc47157, %struct.ScmObj** %stackaddr$prim54116, align 8
%ae49438 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54117 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49438)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim54117, align 8
%stackaddr$prim54118 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47268)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim54118, align 8
%truthy$cmp54119 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47269)
%cmp$cmp54119 = icmp eq i64 %truthy$cmp54119, 1
br i1 %cmp$cmp54119, label %truebranch$cmp54119, label %falsebranch$cmp54119
truebranch$cmp54119:
%ae49442 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49443 = call %struct.ScmObj* @const_init_true()
%argslist53091$k473640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54120 = alloca %struct.ScmObj*, align 8
%argslist53091$k473641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49443, %struct.ScmObj* %argslist53091$k473640)
store volatile %struct.ScmObj* %argslist53091$k473641, %struct.ScmObj** %stackaddr$prim54120, align 8
%stackaddr$prim54121 = alloca %struct.ScmObj*, align 8
%argslist53091$k473642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49442, %struct.ScmObj* %argslist53091$k473641)
store volatile %struct.ScmObj* %argslist53091$k473642, %struct.ScmObj** %stackaddr$prim54121, align 8
%clofunc54122 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47364)
musttail call tailcc void %clofunc54122(%struct.ScmObj* %k47364, %struct.ScmObj* %argslist53091$k473642)
ret void
falsebranch$cmp54119:
%ae49451 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54123 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49451)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim54123, align 8
%stackaddr$prim54124 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47270)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim54124, align 8
%truthy$cmp54125 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47271)
%cmp$cmp54125 = icmp eq i64 %truthy$cmp54125, 1
br i1 %cmp$cmp54125, label %truebranch$cmp54125, label %falsebranch$cmp54125
truebranch$cmp54125:
%ae49455 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54126 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49455)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim54126, align 8
%stackaddr$prim54127 = alloca %struct.ScmObj*, align 8
%b47159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47272)
store volatile %struct.ScmObj* %b47159, %struct.ScmObj** %stackaddr$prim54127, align 8
%ae49458 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54128 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49458)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim54128, align 8
%stackaddr$prim54129 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54129, align 8
%ae49461 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54130 = alloca %struct.ScmObj*, align 8
%_95047160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49461, %struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %_95047160, %struct.ScmObj** %stackaddr$prim54130, align 8
%argslist53092$cc471570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54131 = alloca %struct.ScmObj*, align 8
%argslist53092$cc471571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist53092$cc471570)
store volatile %struct.ScmObj* %argslist53092$cc471571, %struct.ScmObj** %stackaddr$prim54131, align 8
%stackaddr$prim54132 = alloca %struct.ScmObj*, align 8
%argslist53092$cc471572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47364, %struct.ScmObj* %argslist53092$cc471571)
store volatile %struct.ScmObj* %argslist53092$cc471572, %struct.ScmObj** %stackaddr$prim54132, align 8
%clofunc54133 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47157)
musttail call tailcc void %clofunc54133(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist53092$cc471572)
ret void
falsebranch$cmp54125:
%ae49494 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49495 = call %struct.ScmObj* @const_init_false()
%argslist53093$k473640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54134 = alloca %struct.ScmObj*, align 8
%argslist53093$k473641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49495, %struct.ScmObj* %argslist53093$k473640)
store volatile %struct.ScmObj* %argslist53093$k473641, %struct.ScmObj** %stackaddr$prim54134, align 8
%stackaddr$prim54135 = alloca %struct.ScmObj*, align 8
%argslist53093$k473642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49494, %struct.ScmObj* %argslist53093$k473641)
store volatile %struct.ScmObj* %argslist53093$k473642, %struct.ScmObj** %stackaddr$prim54135, align 8
%clofunc54136 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47364)
musttail call tailcc void %clofunc54136(%struct.ScmObj* %k47364, %struct.ScmObj* %argslist53093$k473642)
ret void
}

define tailcc void @proc_clo$ae49323(%struct.ScmObj* %env$ae49323,%struct.ScmObj* %current_45args53094) {
%stackaddr$env-ref54137 = alloca %struct.ScmObj*, align 8
%a47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49323, i64 0)
store %struct.ScmObj* %a47156, %struct.ScmObj** %stackaddr$env-ref54137
%stackaddr$env-ref54138 = alloca %struct.ScmObj*, align 8
%k47364 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49323, i64 1)
store %struct.ScmObj* %k47364, %struct.ScmObj** %stackaddr$env-ref54138
%stackaddr$prim54139 = alloca %struct.ScmObj*, align 8
%_95k47366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53094)
store volatile %struct.ScmObj* %_95k47366, %struct.ScmObj** %stackaddr$prim54139, align 8
%stackaddr$prim54140 = alloca %struct.ScmObj*, align 8
%current_45args53095 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53094)
store volatile %struct.ScmObj* %current_45args53095, %struct.ScmObj** %stackaddr$prim54140, align 8
%stackaddr$prim54141 = alloca %struct.ScmObj*, align 8
%cc47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53095)
store volatile %struct.ScmObj* %cc47157, %struct.ScmObj** %stackaddr$prim54141, align 8
%ae49325 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54142 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49325)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim54142, align 8
%stackaddr$prim54143 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47268)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim54143, align 8
%truthy$cmp54144 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47269)
%cmp$cmp54144 = icmp eq i64 %truthy$cmp54144, 1
br i1 %cmp$cmp54144, label %truebranch$cmp54144, label %falsebranch$cmp54144
truebranch$cmp54144:
%ae49329 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49330 = call %struct.ScmObj* @const_init_true()
%argslist53097$k473640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54145 = alloca %struct.ScmObj*, align 8
%argslist53097$k473641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49330, %struct.ScmObj* %argslist53097$k473640)
store volatile %struct.ScmObj* %argslist53097$k473641, %struct.ScmObj** %stackaddr$prim54145, align 8
%stackaddr$prim54146 = alloca %struct.ScmObj*, align 8
%argslist53097$k473642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49329, %struct.ScmObj* %argslist53097$k473641)
store volatile %struct.ScmObj* %argslist53097$k473642, %struct.ScmObj** %stackaddr$prim54146, align 8
%clofunc54147 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47364)
musttail call tailcc void %clofunc54147(%struct.ScmObj* %k47364, %struct.ScmObj* %argslist53097$k473642)
ret void
falsebranch$cmp54144:
%ae49338 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54148 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49338)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim54148, align 8
%stackaddr$prim54149 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47270)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim54149, align 8
%truthy$cmp54150 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47271)
%cmp$cmp54150 = icmp eq i64 %truthy$cmp54150, 1
br i1 %cmp$cmp54150, label %truebranch$cmp54150, label %falsebranch$cmp54150
truebranch$cmp54150:
%ae49342 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54151 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49342)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim54151, align 8
%stackaddr$prim54152 = alloca %struct.ScmObj*, align 8
%b47159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47272)
store volatile %struct.ScmObj* %b47159, %struct.ScmObj** %stackaddr$prim54152, align 8
%ae49345 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54153 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49345)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim54153, align 8
%stackaddr$prim54154 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54154, align 8
%ae49348 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54155 = alloca %struct.ScmObj*, align 8
%_95047160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49348, %struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %_95047160, %struct.ScmObj** %stackaddr$prim54155, align 8
%argslist53098$cc471570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54156 = alloca %struct.ScmObj*, align 8
%argslist53098$cc471571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist53098$cc471570)
store volatile %struct.ScmObj* %argslist53098$cc471571, %struct.ScmObj** %stackaddr$prim54156, align 8
%stackaddr$prim54157 = alloca %struct.ScmObj*, align 8
%argslist53098$cc471572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47364, %struct.ScmObj* %argslist53098$cc471571)
store volatile %struct.ScmObj* %argslist53098$cc471572, %struct.ScmObj** %stackaddr$prim54157, align 8
%clofunc54158 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47157)
musttail call tailcc void %clofunc54158(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist53098$cc471572)
ret void
falsebranch$cmp54150:
%ae49381 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49382 = call %struct.ScmObj* @const_init_false()
%argslist53099$k473640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54159 = alloca %struct.ScmObj*, align 8
%argslist53099$k473641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49382, %struct.ScmObj* %argslist53099$k473640)
store volatile %struct.ScmObj* %argslist53099$k473641, %struct.ScmObj** %stackaddr$prim54159, align 8
%stackaddr$prim54160 = alloca %struct.ScmObj*, align 8
%argslist53099$k473642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49381, %struct.ScmObj* %argslist53099$k473641)
store volatile %struct.ScmObj* %argslist53099$k473642, %struct.ScmObj** %stackaddr$prim54160, align 8
%clofunc54161 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47364)
musttail call tailcc void %clofunc54161(%struct.ScmObj* %k47364, %struct.ScmObj* %argslist53099$k473642)
ret void
}

define tailcc void @proc_clo$ae49305(%struct.ScmObj* %env$ae49305,%struct.ScmObj* %current_45args53101) {
%stackaddr$prim54162 = alloca %struct.ScmObj*, align 8
%k47367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53101)
store volatile %struct.ScmObj* %k47367, %struct.ScmObj** %stackaddr$prim54162, align 8
%stackaddr$prim54163 = alloca %struct.ScmObj*, align 8
%current_45args53102 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53101)
store volatile %struct.ScmObj* %current_45args53102, %struct.ScmObj** %stackaddr$prim54163, align 8
%stackaddr$prim54164 = alloca %struct.ScmObj*, align 8
%k47158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53102)
store volatile %struct.ScmObj* %k47158, %struct.ScmObj** %stackaddr$prim54164, align 8
%ae49307 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53104$k473670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54165 = alloca %struct.ScmObj*, align 8
%argslist53104$k473671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47158, %struct.ScmObj* %argslist53104$k473670)
store volatile %struct.ScmObj* %argslist53104$k473671, %struct.ScmObj** %stackaddr$prim54165, align 8
%stackaddr$prim54166 = alloca %struct.ScmObj*, align 8
%argslist53104$k473672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49307, %struct.ScmObj* %argslist53104$k473671)
store volatile %struct.ScmObj* %argslist53104$k473672, %struct.ScmObj** %stackaddr$prim54166, align 8
%clofunc54167 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47367)
musttail call tailcc void %clofunc54167(%struct.ScmObj* %k47367, %struct.ScmObj* %argslist53104$k473672)
ret void
}

define tailcc void @proc_clo$ae49228(%struct.ScmObj* %env$ae49228,%struct.ScmObj* %current_45args53107) {
%stackaddr$env-ref54168 = alloca %struct.ScmObj*, align 8
%_37append47162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49228, i64 0)
store %struct.ScmObj* %_37append47162, %struct.ScmObj** %stackaddr$env-ref54168
%stackaddr$prim54169 = alloca %struct.ScmObj*, align 8
%k47368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53107)
store volatile %struct.ScmObj* %k47368, %struct.ScmObj** %stackaddr$prim54169, align 8
%stackaddr$prim54170 = alloca %struct.ScmObj*, align 8
%current_45args53108 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53107)
store volatile %struct.ScmObj* %current_45args53108, %struct.ScmObj** %stackaddr$prim54170, align 8
%stackaddr$prim54171 = alloca %struct.ScmObj*, align 8
%ls047165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53108)
store volatile %struct.ScmObj* %ls047165, %struct.ScmObj** %stackaddr$prim54171, align 8
%stackaddr$prim54172 = alloca %struct.ScmObj*, align 8
%current_45args53109 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53108)
store volatile %struct.ScmObj* %current_45args53109, %struct.ScmObj** %stackaddr$prim54172, align 8
%stackaddr$prim54173 = alloca %struct.ScmObj*, align 8
%ls147164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53109)
store volatile %struct.ScmObj* %ls147164, %struct.ScmObj** %stackaddr$prim54173, align 8
%stackaddr$prim54174 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047165)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim54174, align 8
%truthy$cmp54175 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47261)
%cmp$cmp54175 = icmp eq i64 %truthy$cmp54175, 1
br i1 %cmp$cmp54175, label %truebranch$cmp54175, label %falsebranch$cmp54175
truebranch$cmp54175:
%ae49232 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53111$k473680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54176 = alloca %struct.ScmObj*, align 8
%argslist53111$k473681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147164, %struct.ScmObj* %argslist53111$k473680)
store volatile %struct.ScmObj* %argslist53111$k473681, %struct.ScmObj** %stackaddr$prim54176, align 8
%stackaddr$prim54177 = alloca %struct.ScmObj*, align 8
%argslist53111$k473682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49232, %struct.ScmObj* %argslist53111$k473681)
store volatile %struct.ScmObj* %argslist53111$k473682, %struct.ScmObj** %stackaddr$prim54177, align 8
%clofunc54178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47368)
musttail call tailcc void %clofunc54178(%struct.ScmObj* %k47368, %struct.ScmObj* %argslist53111$k473682)
ret void
falsebranch$cmp54175:
%stackaddr$prim54179 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047165)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim54179, align 8
%ae49239 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54180 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47162, %struct.ScmObj* %ae49239)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim54180, align 8
%stackaddr$prim54181 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047165)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim54181, align 8
%stackaddr$makeclosure54182 = alloca %struct.ScmObj*, align 8
%fptrToInt54183 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49242 to i64
%ae49242 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54183)
store volatile %struct.ScmObj* %ae49242, %struct.ScmObj** %stackaddr$makeclosure54182, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %anf_45bind47262, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %k47368, i64 1)
%argslist53116$anf_45bind472630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54184 = alloca %struct.ScmObj*, align 8
%argslist53116$anf_45bind472631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147164, %struct.ScmObj* %argslist53116$anf_45bind472630)
store volatile %struct.ScmObj* %argslist53116$anf_45bind472631, %struct.ScmObj** %stackaddr$prim54184, align 8
%stackaddr$prim54185 = alloca %struct.ScmObj*, align 8
%argslist53116$anf_45bind472632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47264, %struct.ScmObj* %argslist53116$anf_45bind472631)
store volatile %struct.ScmObj* %argslist53116$anf_45bind472632, %struct.ScmObj** %stackaddr$prim54185, align 8
%stackaddr$prim54186 = alloca %struct.ScmObj*, align 8
%argslist53116$anf_45bind472633 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49242, %struct.ScmObj* %argslist53116$anf_45bind472632)
store volatile %struct.ScmObj* %argslist53116$anf_45bind472633, %struct.ScmObj** %stackaddr$prim54186, align 8
%clofunc54187 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47263)
musttail call tailcc void %clofunc54187(%struct.ScmObj* %anf_45bind47263, %struct.ScmObj* %argslist53116$anf_45bind472633)
ret void
}

define tailcc void @proc_clo$ae49242(%struct.ScmObj* %env$ae49242,%struct.ScmObj* %current_45args53112) {
%stackaddr$env-ref54188 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 0)
store %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$env-ref54188
%stackaddr$env-ref54189 = alloca %struct.ScmObj*, align 8
%k47368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 1)
store %struct.ScmObj* %k47368, %struct.ScmObj** %stackaddr$env-ref54189
%stackaddr$prim54190 = alloca %struct.ScmObj*, align 8
%_95k47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53112)
store volatile %struct.ScmObj* %_95k47369, %struct.ScmObj** %stackaddr$prim54190, align 8
%stackaddr$prim54191 = alloca %struct.ScmObj*, align 8
%current_45args53113 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53112)
store volatile %struct.ScmObj* %current_45args53113, %struct.ScmObj** %stackaddr$prim54191, align 8
%stackaddr$prim54192 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53113)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim54192, align 8
%stackaddr$prim54193 = alloca %struct.ScmObj*, align 8
%cpsprim47370 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47262, %struct.ScmObj* %anf_45bind47265)
store volatile %struct.ScmObj* %cpsprim47370, %struct.ScmObj** %stackaddr$prim54193, align 8
%ae49248 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53115$k473680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54194 = alloca %struct.ScmObj*, align 8
%argslist53115$k473681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47370, %struct.ScmObj* %argslist53115$k473680)
store volatile %struct.ScmObj* %argslist53115$k473681, %struct.ScmObj** %stackaddr$prim54194, align 8
%stackaddr$prim54195 = alloca %struct.ScmObj*, align 8
%argslist53115$k473682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49248, %struct.ScmObj* %argslist53115$k473681)
store volatile %struct.ScmObj* %argslist53115$k473682, %struct.ScmObj** %stackaddr$prim54195, align 8
%clofunc54196 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47368)
musttail call tailcc void %clofunc54196(%struct.ScmObj* %k47368, %struct.ScmObj* %argslist53115$k473682)
ret void
}

define tailcc void @proc_clo$ae49202(%struct.ScmObj* %env$ae49202,%struct.ScmObj* %current_45args53118) {
%stackaddr$prim54197 = alloca %struct.ScmObj*, align 8
%k47371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53118)
store volatile %struct.ScmObj* %k47371, %struct.ScmObj** %stackaddr$prim54197, align 8
%stackaddr$prim54198 = alloca %struct.ScmObj*, align 8
%current_45args53119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53118)
store volatile %struct.ScmObj* %current_45args53119, %struct.ScmObj** %stackaddr$prim54198, align 8
%stackaddr$prim54199 = alloca %struct.ScmObj*, align 8
%a47168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53119)
store volatile %struct.ScmObj* %a47168, %struct.ScmObj** %stackaddr$prim54199, align 8
%stackaddr$prim54200 = alloca %struct.ScmObj*, align 8
%current_45args53120 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53119)
store volatile %struct.ScmObj* %current_45args53120, %struct.ScmObj** %stackaddr$prim54200, align 8
%stackaddr$prim54201 = alloca %struct.ScmObj*, align 8
%b47167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53120)
store volatile %struct.ScmObj* %b47167, %struct.ScmObj** %stackaddr$prim54201, align 8
%stackaddr$prim54202 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47168, %struct.ScmObj* %b47167)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim54202, align 8
%stackaddr$prim54203 = alloca %struct.ScmObj*, align 8
%cpsprim47372 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47260)
store volatile %struct.ScmObj* %cpsprim47372, %struct.ScmObj** %stackaddr$prim54203, align 8
%ae49207 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53122$k473710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54204 = alloca %struct.ScmObj*, align 8
%argslist53122$k473711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47372, %struct.ScmObj* %argslist53122$k473710)
store volatile %struct.ScmObj* %argslist53122$k473711, %struct.ScmObj** %stackaddr$prim54204, align 8
%stackaddr$prim54205 = alloca %struct.ScmObj*, align 8
%argslist53122$k473712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49207, %struct.ScmObj* %argslist53122$k473711)
store volatile %struct.ScmObj* %argslist53122$k473712, %struct.ScmObj** %stackaddr$prim54205, align 8
%clofunc54206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47371)
musttail call tailcc void %clofunc54206(%struct.ScmObj* %k47371, %struct.ScmObj* %argslist53122$k473712)
ret void
}

define tailcc void @proc_clo$ae49178(%struct.ScmObj* %env$ae49178,%struct.ScmObj* %current_45args53124) {
%stackaddr$prim54207 = alloca %struct.ScmObj*, align 8
%k47373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53124)
store volatile %struct.ScmObj* %k47373, %struct.ScmObj** %stackaddr$prim54207, align 8
%stackaddr$prim54208 = alloca %struct.ScmObj*, align 8
%current_45args53125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53124)
store volatile %struct.ScmObj* %current_45args53125, %struct.ScmObj** %stackaddr$prim54208, align 8
%stackaddr$prim54209 = alloca %struct.ScmObj*, align 8
%a47171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53125)
store volatile %struct.ScmObj* %a47171, %struct.ScmObj** %stackaddr$prim54209, align 8
%stackaddr$prim54210 = alloca %struct.ScmObj*, align 8
%current_45args53126 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53125)
store volatile %struct.ScmObj* %current_45args53126, %struct.ScmObj** %stackaddr$prim54210, align 8
%stackaddr$prim54211 = alloca %struct.ScmObj*, align 8
%b47170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53126)
store volatile %struct.ScmObj* %b47170, %struct.ScmObj** %stackaddr$prim54211, align 8
%stackaddr$prim54212 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47171, %struct.ScmObj* %b47170)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim54212, align 8
%stackaddr$prim54213 = alloca %struct.ScmObj*, align 8
%cpsprim47374 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47259)
store volatile %struct.ScmObj* %cpsprim47374, %struct.ScmObj** %stackaddr$prim54213, align 8
%ae49183 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53128$k473730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54214 = alloca %struct.ScmObj*, align 8
%argslist53128$k473731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47374, %struct.ScmObj* %argslist53128$k473730)
store volatile %struct.ScmObj* %argslist53128$k473731, %struct.ScmObj** %stackaddr$prim54214, align 8
%stackaddr$prim54215 = alloca %struct.ScmObj*, align 8
%argslist53128$k473732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49183, %struct.ScmObj* %argslist53128$k473731)
store volatile %struct.ScmObj* %argslist53128$k473732, %struct.ScmObj** %stackaddr$prim54215, align 8
%clofunc54216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47373)
musttail call tailcc void %clofunc54216(%struct.ScmObj* %k47373, %struct.ScmObj* %argslist53128$k473732)
ret void
}

define tailcc void @proc_clo$ae48784(%struct.ScmObj* %env$ae48784,%struct.ScmObj* %current_45args53131) {
%stackaddr$env-ref54217 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48784, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54217
%stackaddr$env-ref54218 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48784, i64 1)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54218
%stackaddr$env-ref54219 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48784, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54219
%stackaddr$prim54220 = alloca %struct.ScmObj*, align 8
%k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53131)
store volatile %struct.ScmObj* %k47375, %struct.ScmObj** %stackaddr$prim54220, align 8
%stackaddr$prim54221 = alloca %struct.ScmObj*, align 8
%current_45args53132 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53131)
store volatile %struct.ScmObj* %current_45args53132, %struct.ScmObj** %stackaddr$prim54221, align 8
%stackaddr$prim54222 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53132)
store volatile %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$prim54222, align 8
%ae48786 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54223 = alloca %struct.ScmObj*, align 8
%fptrToInt54224 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48787 to i64
%ae48787 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54224)
store volatile %struct.ScmObj* %ae48787, %struct.ScmObj** %stackaddr$makeclosure54223, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48787, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48787, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48787, %struct.ScmObj* %_37map147121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48787, %struct.ScmObj* %_37foldr47095, i64 3)
%argslist53189$k473750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54225 = alloca %struct.ScmObj*, align 8
%argslist53189$k473751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48787, %struct.ScmObj* %argslist53189$k473750)
store volatile %struct.ScmObj* %argslist53189$k473751, %struct.ScmObj** %stackaddr$prim54225, align 8
%stackaddr$prim54226 = alloca %struct.ScmObj*, align 8
%argslist53189$k473752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48786, %struct.ScmObj* %argslist53189$k473751)
store volatile %struct.ScmObj* %argslist53189$k473752, %struct.ScmObj** %stackaddr$prim54226, align 8
%clofunc54227 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47375)
musttail call tailcc void %clofunc54227(%struct.ScmObj* %k47375, %struct.ScmObj* %argslist53189$k473752)
ret void
}

define tailcc void @proc_clo$ae48787(%struct.ScmObj* %env$ae48787,%struct.ScmObj* %args4717447376) {
%stackaddr$env-ref54228 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48787, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54228
%stackaddr$env-ref54229 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48787, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54229
%stackaddr$env-ref54230 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48787, i64 2)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54230
%stackaddr$env-ref54231 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48787, i64 3)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54231
%stackaddr$prim54232 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4717447376)
store volatile %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$prim54232, align 8
%stackaddr$prim54233 = alloca %struct.ScmObj*, align 8
%args47174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4717447376)
store volatile %struct.ScmObj* %args47174, %struct.ScmObj** %stackaddr$prim54233, align 8
%stackaddr$prim54234 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47174)
store volatile %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$prim54234, align 8
%stackaddr$prim54235 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47174)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim54235, align 8
%stackaddr$prim54236 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47247)
store volatile %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$prim54236, align 8
%stackaddr$prim54237 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47174)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim54237, align 8
%stackaddr$prim54238 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47248)
store volatile %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$prim54238, align 8
%stackaddr$makeclosure54239 = alloca %struct.ScmObj*, align 8
%fptrToInt54240 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48795 to i64
%ae48795 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54240)
store volatile %struct.ScmObj* %ae48795, %struct.ScmObj** %stackaddr$makeclosure54239, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %k47377, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %_37map147121, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %lsts47175, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %_37foldr47095, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %_37foldl47173, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %_37foldr147090, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %f47177, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %acc47176, i64 7)
%ae48796 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54241 = alloca %struct.ScmObj*, align 8
%fptrToInt54242 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48797 to i64
%ae48797 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54242)
store volatile %struct.ScmObj* %ae48797, %struct.ScmObj** %stackaddr$makeclosure54241, align 8
%argslist53188$ae487950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54243 = alloca %struct.ScmObj*, align 8
%argslist53188$ae487951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48797, %struct.ScmObj* %argslist53188$ae487950)
store volatile %struct.ScmObj* %argslist53188$ae487951, %struct.ScmObj** %stackaddr$prim54243, align 8
%stackaddr$prim54244 = alloca %struct.ScmObj*, align 8
%argslist53188$ae487952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48796, %struct.ScmObj* %argslist53188$ae487951)
store volatile %struct.ScmObj* %argslist53188$ae487952, %struct.ScmObj** %stackaddr$prim54244, align 8
%clofunc54245 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48795)
musttail call tailcc void %clofunc54245(%struct.ScmObj* %ae48795, %struct.ScmObj* %argslist53188$ae487952)
ret void
}

define tailcc void @proc_clo$ae48795(%struct.ScmObj* %env$ae48795,%struct.ScmObj* %current_45args53134) {
%stackaddr$env-ref54246 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 0)
store %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$env-ref54246
%stackaddr$env-ref54247 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 1)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54247
%stackaddr$env-ref54248 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 2)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref54248
%stackaddr$env-ref54249 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 3)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54249
%stackaddr$env-ref54250 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 4)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54250
%stackaddr$env-ref54251 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 5)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54251
%stackaddr$env-ref54252 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 6)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54252
%stackaddr$env-ref54253 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 7)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54253
%stackaddr$prim54254 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53134)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim54254, align 8
%stackaddr$prim54255 = alloca %struct.ScmObj*, align 8
%current_45args53135 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53134)
store volatile %struct.ScmObj* %current_45args53135, %struct.ScmObj** %stackaddr$prim54255, align 8
%stackaddr$prim54256 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53135)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim54256, align 8
%stackaddr$makeclosure54257 = alloca %struct.ScmObj*, align 8
%fptrToInt54258 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48827 to i64
%ae48827 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54258)
store volatile %struct.ScmObj* %ae48827, %struct.ScmObj** %stackaddr$makeclosure54257, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48827, %struct.ScmObj* %k47377, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48827, %struct.ScmObj* %_37map147121, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48827, %struct.ScmObj* %lsts47175, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48827, %struct.ScmObj* %_37foldr47095, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48827, %struct.ScmObj* %_37foldl47173, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48827, %struct.ScmObj* %f47177, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48827, %struct.ScmObj* %acc47176, i64 6)
%ae48829 = call %struct.ScmObj* @const_init_false()
%argslist53181$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54259 = alloca %struct.ScmObj*, align 8
%argslist53181$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47175, %struct.ScmObj* %argslist53181$_37foldr1470900)
store volatile %struct.ScmObj* %argslist53181$_37foldr1470901, %struct.ScmObj** %stackaddr$prim54259, align 8
%stackaddr$prim54260 = alloca %struct.ScmObj*, align 8
%argslist53181$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48829, %struct.ScmObj* %argslist53181$_37foldr1470901)
store volatile %struct.ScmObj* %argslist53181$_37foldr1470902, %struct.ScmObj** %stackaddr$prim54260, align 8
%stackaddr$prim54261 = alloca %struct.ScmObj*, align 8
%argslist53181$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47249, %struct.ScmObj* %argslist53181$_37foldr1470902)
store volatile %struct.ScmObj* %argslist53181$_37foldr1470903, %struct.ScmObj** %stackaddr$prim54261, align 8
%stackaddr$prim54262 = alloca %struct.ScmObj*, align 8
%argslist53181$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48827, %struct.ScmObj* %argslist53181$_37foldr1470903)
store volatile %struct.ScmObj* %argslist53181$_37foldr1470904, %struct.ScmObj** %stackaddr$prim54262, align 8
%clofunc54263 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc54263(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist53181$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae48827(%struct.ScmObj* %env$ae48827,%struct.ScmObj* %current_45args53137) {
%stackaddr$env-ref54264 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48827, i64 0)
store %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$env-ref54264
%stackaddr$env-ref54265 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48827, i64 1)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54265
%stackaddr$env-ref54266 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48827, i64 2)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref54266
%stackaddr$env-ref54267 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48827, i64 3)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54267
%stackaddr$env-ref54268 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48827, i64 4)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54268
%stackaddr$env-ref54269 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48827, i64 5)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54269
%stackaddr$env-ref54270 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48827, i64 6)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54270
%stackaddr$prim54271 = alloca %struct.ScmObj*, align 8
%_95k47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53137)
store volatile %struct.ScmObj* %_95k47379, %struct.ScmObj** %stackaddr$prim54271, align 8
%stackaddr$prim54272 = alloca %struct.ScmObj*, align 8
%current_45args53138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53137)
store volatile %struct.ScmObj* %current_45args53138, %struct.ScmObj** %stackaddr$prim54272, align 8
%stackaddr$prim54273 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53138)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim54273, align 8
%truthy$cmp54274 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47250)
%cmp$cmp54274 = icmp eq i64 %truthy$cmp54274, 1
br i1 %cmp$cmp54274, label %truebranch$cmp54274, label %falsebranch$cmp54274
truebranch$cmp54274:
%ae48838 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53140$k473770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54275 = alloca %struct.ScmObj*, align 8
%argslist53140$k473771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47176, %struct.ScmObj* %argslist53140$k473770)
store volatile %struct.ScmObj* %argslist53140$k473771, %struct.ScmObj** %stackaddr$prim54275, align 8
%stackaddr$prim54276 = alloca %struct.ScmObj*, align 8
%argslist53140$k473772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48838, %struct.ScmObj* %argslist53140$k473771)
store volatile %struct.ScmObj* %argslist53140$k473772, %struct.ScmObj** %stackaddr$prim54276, align 8
%clofunc54277 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47377)
musttail call tailcc void %clofunc54277(%struct.ScmObj* %k47377, %struct.ScmObj* %argslist53140$k473772)
ret void
falsebranch$cmp54274:
%stackaddr$makeclosure54278 = alloca %struct.ScmObj*, align 8
%fptrToInt54279 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48843 to i64
%ae48843 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54279)
store volatile %struct.ScmObj* %ae48843, %struct.ScmObj** %stackaddr$makeclosure54278, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48843, %struct.ScmObj* %k47377, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48843, %struct.ScmObj* %_37map147121, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48843, %struct.ScmObj* %lsts47175, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48843, %struct.ScmObj* %_37foldr47095, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48843, %struct.ScmObj* %_37foldl47173, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48843, %struct.ScmObj* %f47177, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48843, %struct.ScmObj* %acc47176, i64 6)
%ae48844 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54280 = alloca %struct.ScmObj*, align 8
%fptrToInt54281 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48845 to i64
%ae48845 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54281)
store volatile %struct.ScmObj* %ae48845, %struct.ScmObj** %stackaddr$makeclosure54280, align 8
%argslist53180$ae488430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54282 = alloca %struct.ScmObj*, align 8
%argslist53180$ae488431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48845, %struct.ScmObj* %argslist53180$ae488430)
store volatile %struct.ScmObj* %argslist53180$ae488431, %struct.ScmObj** %stackaddr$prim54282, align 8
%stackaddr$prim54283 = alloca %struct.ScmObj*, align 8
%argslist53180$ae488432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48844, %struct.ScmObj* %argslist53180$ae488431)
store volatile %struct.ScmObj* %argslist53180$ae488432, %struct.ScmObj** %stackaddr$prim54283, align 8
%clofunc54284 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48843)
musttail call tailcc void %clofunc54284(%struct.ScmObj* %ae48843, %struct.ScmObj* %argslist53180$ae488432)
ret void
}

define tailcc void @proc_clo$ae48843(%struct.ScmObj* %env$ae48843,%struct.ScmObj* %current_45args53141) {
%stackaddr$env-ref54285 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48843, i64 0)
store %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$env-ref54285
%stackaddr$env-ref54286 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48843, i64 1)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54286
%stackaddr$env-ref54287 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48843, i64 2)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref54287
%stackaddr$env-ref54288 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48843, i64 3)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54288
%stackaddr$env-ref54289 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48843, i64 4)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54289
%stackaddr$env-ref54290 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48843, i64 5)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54290
%stackaddr$env-ref54291 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48843, i64 6)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54291
%stackaddr$prim54292 = alloca %struct.ScmObj*, align 8
%_95k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53141)
store volatile %struct.ScmObj* %_95k47380, %struct.ScmObj** %stackaddr$prim54292, align 8
%stackaddr$prim54293 = alloca %struct.ScmObj*, align 8
%current_45args53142 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53141)
store volatile %struct.ScmObj* %current_45args53142, %struct.ScmObj** %stackaddr$prim54293, align 8
%stackaddr$prim54294 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53142)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim54294, align 8
%stackaddr$makeclosure54295 = alloca %struct.ScmObj*, align 8
%fptrToInt54296 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48864 to i64
%ae48864 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54296)
store volatile %struct.ScmObj* %ae48864, %struct.ScmObj** %stackaddr$makeclosure54295, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %k47377, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %_37map147121, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %lsts47175, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %_37foldr47095, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %_37foldl47173, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %f47177, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %acc47176, i64 6)
%argslist53175$_37map1471210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54297 = alloca %struct.ScmObj*, align 8
%argslist53175$_37map1471211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47175, %struct.ScmObj* %argslist53175$_37map1471210)
store volatile %struct.ScmObj* %argslist53175$_37map1471211, %struct.ScmObj** %stackaddr$prim54297, align 8
%stackaddr$prim54298 = alloca %struct.ScmObj*, align 8
%argslist53175$_37map1471212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47251, %struct.ScmObj* %argslist53175$_37map1471211)
store volatile %struct.ScmObj* %argslist53175$_37map1471212, %struct.ScmObj** %stackaddr$prim54298, align 8
%stackaddr$prim54299 = alloca %struct.ScmObj*, align 8
%argslist53175$_37map1471213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48864, %struct.ScmObj* %argslist53175$_37map1471212)
store volatile %struct.ScmObj* %argslist53175$_37map1471213, %struct.ScmObj** %stackaddr$prim54299, align 8
%clofunc54300 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147121)
musttail call tailcc void %clofunc54300(%struct.ScmObj* %_37map147121, %struct.ScmObj* %argslist53175$_37map1471213)
ret void
}

define tailcc void @proc_clo$ae48864(%struct.ScmObj* %env$ae48864,%struct.ScmObj* %current_45args53144) {
%stackaddr$env-ref54301 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 0)
store %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$env-ref54301
%stackaddr$env-ref54302 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 1)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54302
%stackaddr$env-ref54303 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 2)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref54303
%stackaddr$env-ref54304 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 3)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54304
%stackaddr$env-ref54305 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 4)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54305
%stackaddr$env-ref54306 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 5)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54306
%stackaddr$env-ref54307 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 6)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54307
%stackaddr$prim54308 = alloca %struct.ScmObj*, align 8
%_95k47381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53144)
store volatile %struct.ScmObj* %_95k47381, %struct.ScmObj** %stackaddr$prim54308, align 8
%stackaddr$prim54309 = alloca %struct.ScmObj*, align 8
%current_45args53145 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53144)
store volatile %struct.ScmObj* %current_45args53145, %struct.ScmObj** %stackaddr$prim54309, align 8
%stackaddr$prim54310 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53145)
store volatile %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$prim54310, align 8
%stackaddr$makeclosure54311 = alloca %struct.ScmObj*, align 8
%fptrToInt54312 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48867 to i64
%ae48867 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54312)
store volatile %struct.ScmObj* %ae48867, %struct.ScmObj** %stackaddr$makeclosure54311, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %k47377, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %_37map147121, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %lsts47175, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %_37foldr47095, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %_37foldl47173, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %lsts_4347182, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %f47177, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %acc47176, i64 7)
%ae48868 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54313 = alloca %struct.ScmObj*, align 8
%fptrToInt54314 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48869 to i64
%ae48869 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54314)
store volatile %struct.ScmObj* %ae48869, %struct.ScmObj** %stackaddr$makeclosure54313, align 8
%argslist53174$ae488670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54315 = alloca %struct.ScmObj*, align 8
%argslist53174$ae488671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48869, %struct.ScmObj* %argslist53174$ae488670)
store volatile %struct.ScmObj* %argslist53174$ae488671, %struct.ScmObj** %stackaddr$prim54315, align 8
%stackaddr$prim54316 = alloca %struct.ScmObj*, align 8
%argslist53174$ae488672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48868, %struct.ScmObj* %argslist53174$ae488671)
store volatile %struct.ScmObj* %argslist53174$ae488672, %struct.ScmObj** %stackaddr$prim54316, align 8
%clofunc54317 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48867)
musttail call tailcc void %clofunc54317(%struct.ScmObj* %ae48867, %struct.ScmObj* %argslist53174$ae488672)
ret void
}

define tailcc void @proc_clo$ae48867(%struct.ScmObj* %env$ae48867,%struct.ScmObj* %current_45args53147) {
%stackaddr$env-ref54318 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 0)
store %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$env-ref54318
%stackaddr$env-ref54319 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 1)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54319
%stackaddr$env-ref54320 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 2)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref54320
%stackaddr$env-ref54321 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 3)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54321
%stackaddr$env-ref54322 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 4)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54322
%stackaddr$env-ref54323 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 5)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref54323
%stackaddr$env-ref54324 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 6)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54324
%stackaddr$env-ref54325 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 7)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54325
%stackaddr$prim54326 = alloca %struct.ScmObj*, align 8
%_95k47382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53147)
store volatile %struct.ScmObj* %_95k47382, %struct.ScmObj** %stackaddr$prim54326, align 8
%stackaddr$prim54327 = alloca %struct.ScmObj*, align 8
%current_45args53148 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53147)
store volatile %struct.ScmObj* %current_45args53148, %struct.ScmObj** %stackaddr$prim54327, align 8
%stackaddr$prim54328 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53148)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim54328, align 8
%stackaddr$makeclosure54329 = alloca %struct.ScmObj*, align 8
%fptrToInt54330 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48888 to i64
%ae48888 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54330)
store volatile %struct.ScmObj* %ae48888, %struct.ScmObj** %stackaddr$makeclosure54329, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %k47377, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %lsts_4347182, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %f47177, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %acc47176, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %_37foldr47095, i64 5)
%argslist53169$_37map1471210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54331 = alloca %struct.ScmObj*, align 8
%argslist53169$_37map1471211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47175, %struct.ScmObj* %argslist53169$_37map1471210)
store volatile %struct.ScmObj* %argslist53169$_37map1471211, %struct.ScmObj** %stackaddr$prim54331, align 8
%stackaddr$prim54332 = alloca %struct.ScmObj*, align 8
%argslist53169$_37map1471212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47252, %struct.ScmObj* %argslist53169$_37map1471211)
store volatile %struct.ScmObj* %argslist53169$_37map1471212, %struct.ScmObj** %stackaddr$prim54332, align 8
%stackaddr$prim54333 = alloca %struct.ScmObj*, align 8
%argslist53169$_37map1471213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48888, %struct.ScmObj* %argslist53169$_37map1471212)
store volatile %struct.ScmObj* %argslist53169$_37map1471213, %struct.ScmObj** %stackaddr$prim54333, align 8
%clofunc54334 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147121)
musttail call tailcc void %clofunc54334(%struct.ScmObj* %_37map147121, %struct.ScmObj* %argslist53169$_37map1471213)
ret void
}

define tailcc void @proc_clo$ae48888(%struct.ScmObj* %env$ae48888,%struct.ScmObj* %current_45args53150) {
%stackaddr$env-ref54335 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54335
%stackaddr$env-ref54336 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 1)
store %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$env-ref54336
%stackaddr$env-ref54337 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 2)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref54337
%stackaddr$env-ref54338 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 3)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54338
%stackaddr$env-ref54339 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 4)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54339
%stackaddr$env-ref54340 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 5)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54340
%stackaddr$prim54341 = alloca %struct.ScmObj*, align 8
%_95k47383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53150)
store volatile %struct.ScmObj* %_95k47383, %struct.ScmObj** %stackaddr$prim54341, align 8
%stackaddr$prim54342 = alloca %struct.ScmObj*, align 8
%current_45args53151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53150)
store volatile %struct.ScmObj* %current_45args53151, %struct.ScmObj** %stackaddr$prim54342, align 8
%stackaddr$prim54343 = alloca %struct.ScmObj*, align 8
%vs47180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53151)
store volatile %struct.ScmObj* %vs47180, %struct.ScmObj** %stackaddr$prim54343, align 8
%stackaddr$makeclosure54344 = alloca %struct.ScmObj*, align 8
%fptrToInt54345 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48891 to i64
%ae48891 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54345)
store volatile %struct.ScmObj* %ae48891, %struct.ScmObj** %stackaddr$makeclosure54344, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %k47377, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %lsts_4347182, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %vs47180, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %f47177, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %acc47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48892 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54346 = alloca %struct.ScmObj*, align 8
%fptrToInt54347 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48893 to i64
%ae48893 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54347)
store volatile %struct.ScmObj* %ae48893, %struct.ScmObj** %stackaddr$makeclosure54346, align 8
%argslist53168$ae488910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54348 = alloca %struct.ScmObj*, align 8
%argslist53168$ae488911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48893, %struct.ScmObj* %argslist53168$ae488910)
store volatile %struct.ScmObj* %argslist53168$ae488911, %struct.ScmObj** %stackaddr$prim54348, align 8
%stackaddr$prim54349 = alloca %struct.ScmObj*, align 8
%argslist53168$ae488912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48892, %struct.ScmObj* %argslist53168$ae488911)
store volatile %struct.ScmObj* %argslist53168$ae488912, %struct.ScmObj** %stackaddr$prim54349, align 8
%clofunc54350 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48891)
musttail call tailcc void %clofunc54350(%struct.ScmObj* %ae48891, %struct.ScmObj* %argslist53168$ae488912)
ret void
}

define tailcc void @proc_clo$ae48891(%struct.ScmObj* %env$ae48891,%struct.ScmObj* %current_45args53153) {
%stackaddr$env-ref54351 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54351
%stackaddr$env-ref54352 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 1)
store %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$env-ref54352
%stackaddr$env-ref54353 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 2)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref54353
%stackaddr$env-ref54354 = alloca %struct.ScmObj*, align 8
%vs47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 3)
store %struct.ScmObj* %vs47180, %struct.ScmObj** %stackaddr$env-ref54354
%stackaddr$env-ref54355 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 4)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54355
%stackaddr$env-ref54356 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 5)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54356
%stackaddr$env-ref54357 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54357
%stackaddr$prim54358 = alloca %struct.ScmObj*, align 8
%_95k47384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53153)
store volatile %struct.ScmObj* %_95k47384, %struct.ScmObj** %stackaddr$prim54358, align 8
%stackaddr$prim54359 = alloca %struct.ScmObj*, align 8
%current_45args53154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53153)
store volatile %struct.ScmObj* %current_45args53154, %struct.ScmObj** %stackaddr$prim54359, align 8
%stackaddr$prim54360 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53154)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim54360, align 8
%ae48914 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54361 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47176, %struct.ScmObj* %ae48914)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim54361, align 8
%stackaddr$makeclosure54362 = alloca %struct.ScmObj*, align 8
%fptrToInt54363 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48916 to i64
%ae48916 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54363)
store volatile %struct.ScmObj* %ae48916, %struct.ScmObj** %stackaddr$makeclosure54362, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48916, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48916, %struct.ScmObj* %k47377, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48916, %struct.ScmObj* %lsts_4347182, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48916, %struct.ScmObj* %f47177, i64 3)
%argslist53162$_37foldr470950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54364 = alloca %struct.ScmObj*, align 8
%argslist53162$_37foldr470951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47180, %struct.ScmObj* %argslist53162$_37foldr470950)
store volatile %struct.ScmObj* %argslist53162$_37foldr470951, %struct.ScmObj** %stackaddr$prim54364, align 8
%stackaddr$prim54365 = alloca %struct.ScmObj*, align 8
%argslist53162$_37foldr470952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %argslist53162$_37foldr470951)
store volatile %struct.ScmObj* %argslist53162$_37foldr470952, %struct.ScmObj** %stackaddr$prim54365, align 8
%stackaddr$prim54366 = alloca %struct.ScmObj*, align 8
%argslist53162$_37foldr470953 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47253, %struct.ScmObj* %argslist53162$_37foldr470952)
store volatile %struct.ScmObj* %argslist53162$_37foldr470953, %struct.ScmObj** %stackaddr$prim54366, align 8
%stackaddr$prim54367 = alloca %struct.ScmObj*, align 8
%argslist53162$_37foldr470954 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48916, %struct.ScmObj* %argslist53162$_37foldr470953)
store volatile %struct.ScmObj* %argslist53162$_37foldr470954, %struct.ScmObj** %stackaddr$prim54367, align 8
%clofunc54368 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47095)
musttail call tailcc void %clofunc54368(%struct.ScmObj* %_37foldr47095, %struct.ScmObj* %argslist53162$_37foldr470954)
ret void
}

define tailcc void @proc_clo$ae48916(%struct.ScmObj* %env$ae48916,%struct.ScmObj* %current_45args53156) {
%stackaddr$env-ref54369 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48916, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54369
%stackaddr$env-ref54370 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48916, i64 1)
store %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$env-ref54370
%stackaddr$env-ref54371 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48916, i64 2)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref54371
%stackaddr$env-ref54372 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48916, i64 3)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54372
%stackaddr$prim54373 = alloca %struct.ScmObj*, align 8
%_95k47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53156)
store volatile %struct.ScmObj* %_95k47385, %struct.ScmObj** %stackaddr$prim54373, align 8
%stackaddr$prim54374 = alloca %struct.ScmObj*, align 8
%current_45args53157 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53156)
store volatile %struct.ScmObj* %current_45args53157, %struct.ScmObj** %stackaddr$prim54374, align 8
%stackaddr$prim54375 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53157)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim54375, align 8
%stackaddr$makeclosure54376 = alloca %struct.ScmObj*, align 8
%fptrToInt54377 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48920 to i64
%ae48920 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54377)
store volatile %struct.ScmObj* %ae48920, %struct.ScmObj** %stackaddr$makeclosure54376, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48920, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48920, %struct.ScmObj* %k47377, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48920, %struct.ScmObj* %lsts_4347182, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48920, %struct.ScmObj* %f47177, i64 3)
%stackaddr$prim54378 = alloca %struct.ScmObj*, align 8
%cpsargs47388 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48920, %struct.ScmObj* %anf_45bind47255)
store volatile %struct.ScmObj* %cpsargs47388, %struct.ScmObj** %stackaddr$prim54378, align 8
%clofunc54379 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47177)
musttail call tailcc void %clofunc54379(%struct.ScmObj* %f47177, %struct.ScmObj* %cpsargs47388)
ret void
}

define tailcc void @proc_clo$ae48920(%struct.ScmObj* %env$ae48920,%struct.ScmObj* %current_45args53159) {
%stackaddr$env-ref54380 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48920, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54380
%stackaddr$env-ref54381 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48920, i64 1)
store %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$env-ref54381
%stackaddr$env-ref54382 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48920, i64 2)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref54382
%stackaddr$env-ref54383 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48920, i64 3)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54383
%stackaddr$prim54384 = alloca %struct.ScmObj*, align 8
%_95k47386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53159)
store volatile %struct.ScmObj* %_95k47386, %struct.ScmObj** %stackaddr$prim54384, align 8
%stackaddr$prim54385 = alloca %struct.ScmObj*, align 8
%current_45args53160 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53159)
store volatile %struct.ScmObj* %current_45args53160, %struct.ScmObj** %stackaddr$prim54385, align 8
%stackaddr$prim54386 = alloca %struct.ScmObj*, align 8
%acc_4347184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53160)
store volatile %struct.ScmObj* %acc_4347184, %struct.ScmObj** %stackaddr$prim54386, align 8
%stackaddr$prim54387 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347184, %struct.ScmObj* %lsts_4347182)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim54387, align 8
%stackaddr$prim54388 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47177, %struct.ScmObj* %anf_45bind47256)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim54388, align 8
%stackaddr$prim54389 = alloca %struct.ScmObj*, align 8
%cpsargs47387 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47377, %struct.ScmObj* %anf_45bind47257)
store volatile %struct.ScmObj* %cpsargs47387, %struct.ScmObj** %stackaddr$prim54389, align 8
%clofunc54390 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47173)
musttail call tailcc void %clofunc54390(%struct.ScmObj* %_37foldl47173, %struct.ScmObj* %cpsargs47387)
ret void
}

define tailcc void @proc_clo$ae48893(%struct.ScmObj* %env$ae48893,%struct.ScmObj* %current_45args53163) {
%stackaddr$prim54391 = alloca %struct.ScmObj*, align 8
%k47389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53163)
store volatile %struct.ScmObj* %k47389, %struct.ScmObj** %stackaddr$prim54391, align 8
%stackaddr$prim54392 = alloca %struct.ScmObj*, align 8
%current_45args53164 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53163)
store volatile %struct.ScmObj* %current_45args53164, %struct.ScmObj** %stackaddr$prim54392, align 8
%stackaddr$prim54393 = alloca %struct.ScmObj*, align 8
%a47186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53164)
store volatile %struct.ScmObj* %a47186, %struct.ScmObj** %stackaddr$prim54393, align 8
%stackaddr$prim54394 = alloca %struct.ScmObj*, align 8
%current_45args53165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53164)
store volatile %struct.ScmObj* %current_45args53165, %struct.ScmObj** %stackaddr$prim54394, align 8
%stackaddr$prim54395 = alloca %struct.ScmObj*, align 8
%b47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53165)
store volatile %struct.ScmObj* %b47185, %struct.ScmObj** %stackaddr$prim54395, align 8
%stackaddr$prim54396 = alloca %struct.ScmObj*, align 8
%cpsprim47390 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47186, %struct.ScmObj* %b47185)
store volatile %struct.ScmObj* %cpsprim47390, %struct.ScmObj** %stackaddr$prim54396, align 8
%ae48897 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53167$k473890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54397 = alloca %struct.ScmObj*, align 8
%argslist53167$k473891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47390, %struct.ScmObj* %argslist53167$k473890)
store volatile %struct.ScmObj* %argslist53167$k473891, %struct.ScmObj** %stackaddr$prim54397, align 8
%stackaddr$prim54398 = alloca %struct.ScmObj*, align 8
%argslist53167$k473892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48897, %struct.ScmObj* %argslist53167$k473891)
store volatile %struct.ScmObj* %argslist53167$k473892, %struct.ScmObj** %stackaddr$prim54398, align 8
%clofunc54399 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47389)
musttail call tailcc void %clofunc54399(%struct.ScmObj* %k47389, %struct.ScmObj* %argslist53167$k473892)
ret void
}

define tailcc void @proc_clo$ae48869(%struct.ScmObj* %env$ae48869,%struct.ScmObj* %current_45args53170) {
%stackaddr$prim54400 = alloca %struct.ScmObj*, align 8
%k47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53170)
store volatile %struct.ScmObj* %k47391, %struct.ScmObj** %stackaddr$prim54400, align 8
%stackaddr$prim54401 = alloca %struct.ScmObj*, align 8
%current_45args53171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53170)
store volatile %struct.ScmObj* %current_45args53171, %struct.ScmObj** %stackaddr$prim54401, align 8
%stackaddr$prim54402 = alloca %struct.ScmObj*, align 8
%x47181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53171)
store volatile %struct.ScmObj* %x47181, %struct.ScmObj** %stackaddr$prim54402, align 8
%stackaddr$prim54403 = alloca %struct.ScmObj*, align 8
%cpsprim47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47181)
store volatile %struct.ScmObj* %cpsprim47392, %struct.ScmObj** %stackaddr$prim54403, align 8
%ae48872 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53173$k473910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54404 = alloca %struct.ScmObj*, align 8
%argslist53173$k473911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47392, %struct.ScmObj* %argslist53173$k473910)
store volatile %struct.ScmObj* %argslist53173$k473911, %struct.ScmObj** %stackaddr$prim54404, align 8
%stackaddr$prim54405 = alloca %struct.ScmObj*, align 8
%argslist53173$k473912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48872, %struct.ScmObj* %argslist53173$k473911)
store volatile %struct.ScmObj* %argslist53173$k473912, %struct.ScmObj** %stackaddr$prim54405, align 8
%clofunc54406 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47391)
musttail call tailcc void %clofunc54406(%struct.ScmObj* %k47391, %struct.ScmObj* %argslist53173$k473912)
ret void
}

define tailcc void @proc_clo$ae48845(%struct.ScmObj* %env$ae48845,%struct.ScmObj* %current_45args53176) {
%stackaddr$prim54407 = alloca %struct.ScmObj*, align 8
%k47393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53176)
store volatile %struct.ScmObj* %k47393, %struct.ScmObj** %stackaddr$prim54407, align 8
%stackaddr$prim54408 = alloca %struct.ScmObj*, align 8
%current_45args53177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53176)
store volatile %struct.ScmObj* %current_45args53177, %struct.ScmObj** %stackaddr$prim54408, align 8
%stackaddr$prim54409 = alloca %struct.ScmObj*, align 8
%x47183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53177)
store volatile %struct.ScmObj* %x47183, %struct.ScmObj** %stackaddr$prim54409, align 8
%stackaddr$prim54410 = alloca %struct.ScmObj*, align 8
%cpsprim47394 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47183)
store volatile %struct.ScmObj* %cpsprim47394, %struct.ScmObj** %stackaddr$prim54410, align 8
%ae48848 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53179$k473930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54411 = alloca %struct.ScmObj*, align 8
%argslist53179$k473931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47394, %struct.ScmObj* %argslist53179$k473930)
store volatile %struct.ScmObj* %argslist53179$k473931, %struct.ScmObj** %stackaddr$prim54411, align 8
%stackaddr$prim54412 = alloca %struct.ScmObj*, align 8
%argslist53179$k473932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48848, %struct.ScmObj* %argslist53179$k473931)
store volatile %struct.ScmObj* %argslist53179$k473932, %struct.ScmObj** %stackaddr$prim54412, align 8
%clofunc54413 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47393)
musttail call tailcc void %clofunc54413(%struct.ScmObj* %k47393, %struct.ScmObj* %argslist53179$k473932)
ret void
}

define tailcc void @proc_clo$ae48797(%struct.ScmObj* %env$ae48797,%struct.ScmObj* %current_45args53182) {
%stackaddr$prim54414 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53182)
store volatile %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$prim54414, align 8
%stackaddr$prim54415 = alloca %struct.ScmObj*, align 8
%current_45args53183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53182)
store volatile %struct.ScmObj* %current_45args53183, %struct.ScmObj** %stackaddr$prim54415, align 8
%stackaddr$prim54416 = alloca %struct.ScmObj*, align 8
%lst47179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53183)
store volatile %struct.ScmObj* %lst47179, %struct.ScmObj** %stackaddr$prim54416, align 8
%stackaddr$prim54417 = alloca %struct.ScmObj*, align 8
%current_45args53184 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53183)
store volatile %struct.ScmObj* %current_45args53184, %struct.ScmObj** %stackaddr$prim54417, align 8
%stackaddr$prim54418 = alloca %struct.ScmObj*, align 8
%b47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53184)
store volatile %struct.ScmObj* %b47178, %struct.ScmObj** %stackaddr$prim54418, align 8
%truthy$cmp54419 = call i64 @is_truthy_value(%struct.ScmObj* %b47178)
%cmp$cmp54419 = icmp eq i64 %truthy$cmp54419, 1
br i1 %cmp$cmp54419, label %truebranch$cmp54419, label %falsebranch$cmp54419
truebranch$cmp54419:
%ae48800 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53186$k473950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54420 = alloca %struct.ScmObj*, align 8
%argslist53186$k473951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47178, %struct.ScmObj* %argslist53186$k473950)
store volatile %struct.ScmObj* %argslist53186$k473951, %struct.ScmObj** %stackaddr$prim54420, align 8
%stackaddr$prim54421 = alloca %struct.ScmObj*, align 8
%argslist53186$k473952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48800, %struct.ScmObj* %argslist53186$k473951)
store volatile %struct.ScmObj* %argslist53186$k473952, %struct.ScmObj** %stackaddr$prim54421, align 8
%clofunc54422 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47395)
musttail call tailcc void %clofunc54422(%struct.ScmObj* %k47395, %struct.ScmObj* %argslist53186$k473952)
ret void
falsebranch$cmp54419:
%stackaddr$prim54423 = alloca %struct.ScmObj*, align 8
%cpsprim47396 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47179)
store volatile %struct.ScmObj* %cpsprim47396, %struct.ScmObj** %stackaddr$prim54423, align 8
%ae48807 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53187$k473950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54424 = alloca %struct.ScmObj*, align 8
%argslist53187$k473951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47396, %struct.ScmObj* %argslist53187$k473950)
store volatile %struct.ScmObj* %argslist53187$k473951, %struct.ScmObj** %stackaddr$prim54424, align 8
%stackaddr$prim54425 = alloca %struct.ScmObj*, align 8
%argslist53187$k473952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48807, %struct.ScmObj* %argslist53187$k473951)
store volatile %struct.ScmObj* %argslist53187$k473952, %struct.ScmObj** %stackaddr$prim54425, align 8
%clofunc54426 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47395)
musttail call tailcc void %clofunc54426(%struct.ScmObj* %k47395, %struct.ScmObj* %argslist53187$k473952)
ret void
}

define tailcc void @proc_clo$ae48638(%struct.ScmObj* %env$ae48638,%struct.ScmObj* %args4711747397) {
%stackaddr$env-ref54427 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48638, i64 0)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref54427
%stackaddr$env-ref54428 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48638, i64 1)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref54428
%stackaddr$env-ref54429 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48638, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54429
%stackaddr$prim54430 = alloca %struct.ScmObj*, align 8
%k47398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4711747397)
store volatile %struct.ScmObj* %k47398, %struct.ScmObj** %stackaddr$prim54430, align 8
%stackaddr$prim54431 = alloca %struct.ScmObj*, align 8
%args47117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4711747397)
store volatile %struct.ScmObj* %args47117, %struct.ScmObj** %stackaddr$prim54431, align 8
%stackaddr$prim54432 = alloca %struct.ScmObj*, align 8
%f47119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47117)
store volatile %struct.ScmObj* %f47119, %struct.ScmObj** %stackaddr$prim54432, align 8
%stackaddr$prim54433 = alloca %struct.ScmObj*, align 8
%lsts47118 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47117)
store volatile %struct.ScmObj* %lsts47118, %struct.ScmObj** %stackaddr$prim54433, align 8
%stackaddr$makeclosure54434 = alloca %struct.ScmObj*, align 8
%fptrToInt54435 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48643 to i64
%ae48643 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54435)
store volatile %struct.ScmObj* %ae48643, %struct.ScmObj** %stackaddr$makeclosure54434, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48643, %struct.ScmObj* %k47398, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48643, %struct.ScmObj* %lsts47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48643, %struct.ScmObj* %_37foldr47095, i64 2)
%ae48644 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54436 = alloca %struct.ScmObj*, align 8
%fptrToInt54437 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48645 to i64
%ae48645 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54437)
store volatile %struct.ScmObj* %ae48645, %struct.ScmObj** %stackaddr$makeclosure54436, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48645, %struct.ScmObj* %_37drop_45right47109, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48645, %struct.ScmObj* %f47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48645, %struct.ScmObj* %_37last47112, i64 2)
%argslist53206$ae486430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54438 = alloca %struct.ScmObj*, align 8
%argslist53206$ae486431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48645, %struct.ScmObj* %argslist53206$ae486430)
store volatile %struct.ScmObj* %argslist53206$ae486431, %struct.ScmObj** %stackaddr$prim54438, align 8
%stackaddr$prim54439 = alloca %struct.ScmObj*, align 8
%argslist53206$ae486432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48644, %struct.ScmObj* %argslist53206$ae486431)
store volatile %struct.ScmObj* %argslist53206$ae486432, %struct.ScmObj** %stackaddr$prim54439, align 8
%clofunc54440 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48643)
musttail call tailcc void %clofunc54440(%struct.ScmObj* %ae48643, %struct.ScmObj* %argslist53206$ae486432)
ret void
}

define tailcc void @proc_clo$ae48643(%struct.ScmObj* %env$ae48643,%struct.ScmObj* %current_45args53191) {
%stackaddr$env-ref54441 = alloca %struct.ScmObj*, align 8
%k47398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48643, i64 0)
store %struct.ScmObj* %k47398, %struct.ScmObj** %stackaddr$env-ref54441
%stackaddr$env-ref54442 = alloca %struct.ScmObj*, align 8
%lsts47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48643, i64 1)
store %struct.ScmObj* %lsts47118, %struct.ScmObj** %stackaddr$env-ref54442
%stackaddr$env-ref54443 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48643, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54443
%stackaddr$prim54444 = alloca %struct.ScmObj*, align 8
%_95k47399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53191)
store volatile %struct.ScmObj* %_95k47399, %struct.ScmObj** %stackaddr$prim54444, align 8
%stackaddr$prim54445 = alloca %struct.ScmObj*, align 8
%current_45args53192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53191)
store volatile %struct.ScmObj* %current_45args53192, %struct.ScmObj** %stackaddr$prim54445, align 8
%stackaddr$prim54446 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53192)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim54446, align 8
%ae48706 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54447 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48706, %struct.ScmObj* %lsts47118)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim54447, align 8
%stackaddr$prim54448 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47244, %struct.ScmObj* %anf_45bind47245)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim54448, align 8
%stackaddr$prim54449 = alloca %struct.ScmObj*, align 8
%cpsargs47400 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47398, %struct.ScmObj* %anf_45bind47246)
store volatile %struct.ScmObj* %cpsargs47400, %struct.ScmObj** %stackaddr$prim54449, align 8
%clofunc54450 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47095)
musttail call tailcc void %clofunc54450(%struct.ScmObj* %_37foldr47095, %struct.ScmObj* %cpsargs47400)
ret void
}

define tailcc void @proc_clo$ae48645(%struct.ScmObj* %env$ae48645,%struct.ScmObj* %fargs4712047401) {
%stackaddr$env-ref54451 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48645, i64 0)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref54451
%stackaddr$env-ref54452 = alloca %struct.ScmObj*, align 8
%f47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48645, i64 1)
store %struct.ScmObj* %f47119, %struct.ScmObj** %stackaddr$env-ref54452
%stackaddr$env-ref54453 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48645, i64 2)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref54453
%stackaddr$prim54454 = alloca %struct.ScmObj*, align 8
%k47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4712047401)
store volatile %struct.ScmObj* %k47402, %struct.ScmObj** %stackaddr$prim54454, align 8
%stackaddr$prim54455 = alloca %struct.ScmObj*, align 8
%fargs47120 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4712047401)
store volatile %struct.ScmObj* %fargs47120, %struct.ScmObj** %stackaddr$prim54455, align 8
%stackaddr$makeclosure54456 = alloca %struct.ScmObj*, align 8
%fptrToInt54457 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48649 to i64
%ae48649 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54457)
store volatile %struct.ScmObj* %ae48649, %struct.ScmObj** %stackaddr$makeclosure54456, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48649, %struct.ScmObj* %fargs47120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48649, %struct.ScmObj* %f47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48649, %struct.ScmObj* %k47402, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48649, %struct.ScmObj* %_37last47112, i64 3)
%ae48651 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53205$_37drop_45right471090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54458 = alloca %struct.ScmObj*, align 8
%argslist53205$_37drop_45right471091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48651, %struct.ScmObj* %argslist53205$_37drop_45right471090)
store volatile %struct.ScmObj* %argslist53205$_37drop_45right471091, %struct.ScmObj** %stackaddr$prim54458, align 8
%stackaddr$prim54459 = alloca %struct.ScmObj*, align 8
%argslist53205$_37drop_45right471092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47120, %struct.ScmObj* %argslist53205$_37drop_45right471091)
store volatile %struct.ScmObj* %argslist53205$_37drop_45right471092, %struct.ScmObj** %stackaddr$prim54459, align 8
%stackaddr$prim54460 = alloca %struct.ScmObj*, align 8
%argslist53205$_37drop_45right471093 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48649, %struct.ScmObj* %argslist53205$_37drop_45right471092)
store volatile %struct.ScmObj* %argslist53205$_37drop_45right471093, %struct.ScmObj** %stackaddr$prim54460, align 8
%clofunc54461 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47109)
musttail call tailcc void %clofunc54461(%struct.ScmObj* %_37drop_45right47109, %struct.ScmObj* %argslist53205$_37drop_45right471093)
ret void
}

define tailcc void @proc_clo$ae48649(%struct.ScmObj* %env$ae48649,%struct.ScmObj* %current_45args53194) {
%stackaddr$env-ref54462 = alloca %struct.ScmObj*, align 8
%fargs47120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48649, i64 0)
store %struct.ScmObj* %fargs47120, %struct.ScmObj** %stackaddr$env-ref54462
%stackaddr$env-ref54463 = alloca %struct.ScmObj*, align 8
%f47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48649, i64 1)
store %struct.ScmObj* %f47119, %struct.ScmObj** %stackaddr$env-ref54463
%stackaddr$env-ref54464 = alloca %struct.ScmObj*, align 8
%k47402 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48649, i64 2)
store %struct.ScmObj* %k47402, %struct.ScmObj** %stackaddr$env-ref54464
%stackaddr$env-ref54465 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48649, i64 3)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref54465
%stackaddr$prim54466 = alloca %struct.ScmObj*, align 8
%_95k47403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53194)
store volatile %struct.ScmObj* %_95k47403, %struct.ScmObj** %stackaddr$prim54466, align 8
%stackaddr$prim54467 = alloca %struct.ScmObj*, align 8
%current_45args53195 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53194)
store volatile %struct.ScmObj* %current_45args53195, %struct.ScmObj** %stackaddr$prim54467, align 8
%stackaddr$prim54468 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53195)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim54468, align 8
%stackaddr$makeclosure54469 = alloca %struct.ScmObj*, align 8
%fptrToInt54470 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48656 to i64
%ae48656 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54470)
store volatile %struct.ScmObj* %ae48656, %struct.ScmObj** %stackaddr$makeclosure54469, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %fargs47120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %k47402, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %_37last47112, i64 2)
%stackaddr$prim54471 = alloca %struct.ScmObj*, align 8
%cpsargs47407 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48656, %struct.ScmObj* %anf_45bind47241)
store volatile %struct.ScmObj* %cpsargs47407, %struct.ScmObj** %stackaddr$prim54471, align 8
%clofunc54472 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47119)
musttail call tailcc void %clofunc54472(%struct.ScmObj* %f47119, %struct.ScmObj* %cpsargs47407)
ret void
}

define tailcc void @proc_clo$ae48656(%struct.ScmObj* %env$ae48656,%struct.ScmObj* %current_45args53197) {
%stackaddr$env-ref54473 = alloca %struct.ScmObj*, align 8
%fargs47120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 0)
store %struct.ScmObj* %fargs47120, %struct.ScmObj** %stackaddr$env-ref54473
%stackaddr$env-ref54474 = alloca %struct.ScmObj*, align 8
%k47402 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 1)
store %struct.ScmObj* %k47402, %struct.ScmObj** %stackaddr$env-ref54474
%stackaddr$env-ref54475 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 2)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref54475
%stackaddr$prim54476 = alloca %struct.ScmObj*, align 8
%_95k47404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53197)
store volatile %struct.ScmObj* %_95k47404, %struct.ScmObj** %stackaddr$prim54476, align 8
%stackaddr$prim54477 = alloca %struct.ScmObj*, align 8
%current_45args53198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53197)
store volatile %struct.ScmObj* %current_45args53198, %struct.ScmObj** %stackaddr$prim54477, align 8
%stackaddr$prim54478 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53198)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim54478, align 8
%stackaddr$makeclosure54479 = alloca %struct.ScmObj*, align 8
%fptrToInt54480 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48661 to i64
%ae48661 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54480)
store volatile %struct.ScmObj* %ae48661, %struct.ScmObj** %stackaddr$makeclosure54479, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48661, %struct.ScmObj* %k47402, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48661, %struct.ScmObj* %anf_45bind47242, i64 1)
%argslist53204$_37last471120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54481 = alloca %struct.ScmObj*, align 8
%argslist53204$_37last471121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47120, %struct.ScmObj* %argslist53204$_37last471120)
store volatile %struct.ScmObj* %argslist53204$_37last471121, %struct.ScmObj** %stackaddr$prim54481, align 8
%stackaddr$prim54482 = alloca %struct.ScmObj*, align 8
%argslist53204$_37last471122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48661, %struct.ScmObj* %argslist53204$_37last471121)
store volatile %struct.ScmObj* %argslist53204$_37last471122, %struct.ScmObj** %stackaddr$prim54482, align 8
%clofunc54483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47112)
musttail call tailcc void %clofunc54483(%struct.ScmObj* %_37last47112, %struct.ScmObj* %argslist53204$_37last471122)
ret void
}

define tailcc void @proc_clo$ae48661(%struct.ScmObj* %env$ae48661,%struct.ScmObj* %current_45args53200) {
%stackaddr$env-ref54484 = alloca %struct.ScmObj*, align 8
%k47402 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48661, i64 0)
store %struct.ScmObj* %k47402, %struct.ScmObj** %stackaddr$env-ref54484
%stackaddr$env-ref54485 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48661, i64 1)
store %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$env-ref54485
%stackaddr$prim54486 = alloca %struct.ScmObj*, align 8
%_95k47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53200)
store volatile %struct.ScmObj* %_95k47405, %struct.ScmObj** %stackaddr$prim54486, align 8
%stackaddr$prim54487 = alloca %struct.ScmObj*, align 8
%current_45args53201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53200)
store volatile %struct.ScmObj* %current_45args53201, %struct.ScmObj** %stackaddr$prim54487, align 8
%stackaddr$prim54488 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53201)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim54488, align 8
%stackaddr$prim54489 = alloca %struct.ScmObj*, align 8
%cpsprim47406 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47242, %struct.ScmObj* %anf_45bind47243)
store volatile %struct.ScmObj* %cpsprim47406, %struct.ScmObj** %stackaddr$prim54489, align 8
%ae48666 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53203$k474020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54490 = alloca %struct.ScmObj*, align 8
%argslist53203$k474021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47406, %struct.ScmObj* %argslist53203$k474020)
store volatile %struct.ScmObj* %argslist53203$k474021, %struct.ScmObj** %stackaddr$prim54490, align 8
%stackaddr$prim54491 = alloca %struct.ScmObj*, align 8
%argslist53203$k474022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48666, %struct.ScmObj* %argslist53203$k474021)
store volatile %struct.ScmObj* %argslist53203$k474022, %struct.ScmObj** %stackaddr$prim54491, align 8
%clofunc54492 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47402)
musttail call tailcc void %clofunc54492(%struct.ScmObj* %k47402, %struct.ScmObj* %argslist53203$k474022)
ret void
}

define tailcc void @proc_clo$ae48561(%struct.ScmObj* %env$ae48561,%struct.ScmObj* %current_45args53208) {
%stackaddr$env-ref54493 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48561, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54493
%stackaddr$prim54494 = alloca %struct.ScmObj*, align 8
%k47408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53208)
store volatile %struct.ScmObj* %k47408, %struct.ScmObj** %stackaddr$prim54494, align 8
%stackaddr$prim54495 = alloca %struct.ScmObj*, align 8
%current_45args53209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53208)
store volatile %struct.ScmObj* %current_45args53209, %struct.ScmObj** %stackaddr$prim54495, align 8
%stackaddr$prim54496 = alloca %struct.ScmObj*, align 8
%f47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53209)
store volatile %struct.ScmObj* %f47123, %struct.ScmObj** %stackaddr$prim54496, align 8
%stackaddr$prim54497 = alloca %struct.ScmObj*, align 8
%current_45args53210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53209)
store volatile %struct.ScmObj* %current_45args53210, %struct.ScmObj** %stackaddr$prim54497, align 8
%stackaddr$prim54498 = alloca %struct.ScmObj*, align 8
%lst47122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53210)
store volatile %struct.ScmObj* %lst47122, %struct.ScmObj** %stackaddr$prim54498, align 8
%stackaddr$makeclosure54499 = alloca %struct.ScmObj*, align 8
%fptrToInt54500 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48562 to i64
%ae48562 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54500)
store volatile %struct.ScmObj* %ae48562, %struct.ScmObj** %stackaddr$makeclosure54499, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48562, %struct.ScmObj* %lst47122, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48562, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48562, %struct.ScmObj* %k47408, i64 2)
%ae48563 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54501 = alloca %struct.ScmObj*, align 8
%fptrToInt54502 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48564 to i64
%ae48564 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54502)
store volatile %struct.ScmObj* %ae48564, %struct.ScmObj** %stackaddr$makeclosure54501, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48564, %struct.ScmObj* %f47123, i64 0)
%argslist53225$ae485620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54503 = alloca %struct.ScmObj*, align 8
%argslist53225$ae485621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48564, %struct.ScmObj* %argslist53225$ae485620)
store volatile %struct.ScmObj* %argslist53225$ae485621, %struct.ScmObj** %stackaddr$prim54503, align 8
%stackaddr$prim54504 = alloca %struct.ScmObj*, align 8
%argslist53225$ae485622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48563, %struct.ScmObj* %argslist53225$ae485621)
store volatile %struct.ScmObj* %argslist53225$ae485622, %struct.ScmObj** %stackaddr$prim54504, align 8
%clofunc54505 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48562)
musttail call tailcc void %clofunc54505(%struct.ScmObj* %ae48562, %struct.ScmObj* %argslist53225$ae485622)
ret void
}

define tailcc void @proc_clo$ae48562(%struct.ScmObj* %env$ae48562,%struct.ScmObj* %current_45args53212) {
%stackaddr$env-ref54506 = alloca %struct.ScmObj*, align 8
%lst47122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48562, i64 0)
store %struct.ScmObj* %lst47122, %struct.ScmObj** %stackaddr$env-ref54506
%stackaddr$env-ref54507 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48562, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54507
%stackaddr$env-ref54508 = alloca %struct.ScmObj*, align 8
%k47408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48562, i64 2)
store %struct.ScmObj* %k47408, %struct.ScmObj** %stackaddr$env-ref54508
%stackaddr$prim54509 = alloca %struct.ScmObj*, align 8
%_95k47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53212)
store volatile %struct.ScmObj* %_95k47409, %struct.ScmObj** %stackaddr$prim54509, align 8
%stackaddr$prim54510 = alloca %struct.ScmObj*, align 8
%current_45args53213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53212)
store volatile %struct.ScmObj* %current_45args53213, %struct.ScmObj** %stackaddr$prim54510, align 8
%stackaddr$prim54511 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53213)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim54511, align 8
%ae48596 = call %struct.ScmObj* @const_init_null()
%argslist53215$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54512 = alloca %struct.ScmObj*, align 8
%argslist53215$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47122, %struct.ScmObj* %argslist53215$_37foldr1470900)
store volatile %struct.ScmObj* %argslist53215$_37foldr1470901, %struct.ScmObj** %stackaddr$prim54512, align 8
%stackaddr$prim54513 = alloca %struct.ScmObj*, align 8
%argslist53215$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48596, %struct.ScmObj* %argslist53215$_37foldr1470901)
store volatile %struct.ScmObj* %argslist53215$_37foldr1470902, %struct.ScmObj** %stackaddr$prim54513, align 8
%stackaddr$prim54514 = alloca %struct.ScmObj*, align 8
%argslist53215$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47240, %struct.ScmObj* %argslist53215$_37foldr1470902)
store volatile %struct.ScmObj* %argslist53215$_37foldr1470903, %struct.ScmObj** %stackaddr$prim54514, align 8
%stackaddr$prim54515 = alloca %struct.ScmObj*, align 8
%argslist53215$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47408, %struct.ScmObj* %argslist53215$_37foldr1470903)
store volatile %struct.ScmObj* %argslist53215$_37foldr1470904, %struct.ScmObj** %stackaddr$prim54515, align 8
%clofunc54516 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc54516(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist53215$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae48564(%struct.ScmObj* %env$ae48564,%struct.ScmObj* %current_45args53216) {
%stackaddr$env-ref54517 = alloca %struct.ScmObj*, align 8
%f47123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48564, i64 0)
store %struct.ScmObj* %f47123, %struct.ScmObj** %stackaddr$env-ref54517
%stackaddr$prim54518 = alloca %struct.ScmObj*, align 8
%k47410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53216)
store volatile %struct.ScmObj* %k47410, %struct.ScmObj** %stackaddr$prim54518, align 8
%stackaddr$prim54519 = alloca %struct.ScmObj*, align 8
%current_45args53217 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53216)
store volatile %struct.ScmObj* %current_45args53217, %struct.ScmObj** %stackaddr$prim54519, align 8
%stackaddr$prim54520 = alloca %struct.ScmObj*, align 8
%v47125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53217)
store volatile %struct.ScmObj* %v47125, %struct.ScmObj** %stackaddr$prim54520, align 8
%stackaddr$prim54521 = alloca %struct.ScmObj*, align 8
%current_45args53218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53217)
store volatile %struct.ScmObj* %current_45args53218, %struct.ScmObj** %stackaddr$prim54521, align 8
%stackaddr$prim54522 = alloca %struct.ScmObj*, align 8
%r47124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53218)
store volatile %struct.ScmObj* %r47124, %struct.ScmObj** %stackaddr$prim54522, align 8
%stackaddr$makeclosure54523 = alloca %struct.ScmObj*, align 8
%fptrToInt54524 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48566 to i64
%ae48566 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54524)
store volatile %struct.ScmObj* %ae48566, %struct.ScmObj** %stackaddr$makeclosure54523, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48566, %struct.ScmObj* %r47124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48566, %struct.ScmObj* %k47410, i64 1)
%argslist53224$f471230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54525 = alloca %struct.ScmObj*, align 8
%argslist53224$f471231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47125, %struct.ScmObj* %argslist53224$f471230)
store volatile %struct.ScmObj* %argslist53224$f471231, %struct.ScmObj** %stackaddr$prim54525, align 8
%stackaddr$prim54526 = alloca %struct.ScmObj*, align 8
%argslist53224$f471232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48566, %struct.ScmObj* %argslist53224$f471231)
store volatile %struct.ScmObj* %argslist53224$f471232, %struct.ScmObj** %stackaddr$prim54526, align 8
%clofunc54527 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47123)
musttail call tailcc void %clofunc54527(%struct.ScmObj* %f47123, %struct.ScmObj* %argslist53224$f471232)
ret void
}

define tailcc void @proc_clo$ae48566(%struct.ScmObj* %env$ae48566,%struct.ScmObj* %current_45args53220) {
%stackaddr$env-ref54528 = alloca %struct.ScmObj*, align 8
%r47124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48566, i64 0)
store %struct.ScmObj* %r47124, %struct.ScmObj** %stackaddr$env-ref54528
%stackaddr$env-ref54529 = alloca %struct.ScmObj*, align 8
%k47410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48566, i64 1)
store %struct.ScmObj* %k47410, %struct.ScmObj** %stackaddr$env-ref54529
%stackaddr$prim54530 = alloca %struct.ScmObj*, align 8
%_95k47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53220)
store volatile %struct.ScmObj* %_95k47411, %struct.ScmObj** %stackaddr$prim54530, align 8
%stackaddr$prim54531 = alloca %struct.ScmObj*, align 8
%current_45args53221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53220)
store volatile %struct.ScmObj* %current_45args53221, %struct.ScmObj** %stackaddr$prim54531, align 8
%stackaddr$prim54532 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53221)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim54532, align 8
%stackaddr$prim54533 = alloca %struct.ScmObj*, align 8
%cpsprim47412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47239, %struct.ScmObj* %r47124)
store volatile %struct.ScmObj* %cpsprim47412, %struct.ScmObj** %stackaddr$prim54533, align 8
%ae48571 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53223$k474100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54534 = alloca %struct.ScmObj*, align 8
%argslist53223$k474101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47412, %struct.ScmObj* %argslist53223$k474100)
store volatile %struct.ScmObj* %argslist53223$k474101, %struct.ScmObj** %stackaddr$prim54534, align 8
%stackaddr$prim54535 = alloca %struct.ScmObj*, align 8
%argslist53223$k474102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48571, %struct.ScmObj* %argslist53223$k474101)
store volatile %struct.ScmObj* %argslist53223$k474102, %struct.ScmObj** %stackaddr$prim54535, align 8
%clofunc54536 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47410)
musttail call tailcc void %clofunc54536(%struct.ScmObj* %k47410, %struct.ScmObj* %argslist53223$k474102)
ret void
}

define tailcc void @proc_clo$ae48175(%struct.ScmObj* %env$ae48175,%struct.ScmObj* %current_45args53228) {
%stackaddr$env-ref54537 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48175, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54537
%stackaddr$env-ref54538 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48175, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54538
%stackaddr$prim54539 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53228)
store volatile %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$prim54539, align 8
%stackaddr$prim54540 = alloca %struct.ScmObj*, align 8
%current_45args53229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53228)
store volatile %struct.ScmObj* %current_45args53229, %struct.ScmObj** %stackaddr$prim54540, align 8
%stackaddr$prim54541 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53229)
store volatile %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$prim54541, align 8
%ae48177 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54542 = alloca %struct.ScmObj*, align 8
%fptrToInt54543 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48178 to i64
%ae48178 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54543)
store volatile %struct.ScmObj* %ae48178, %struct.ScmObj** %stackaddr$makeclosure54542, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48178, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48178, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48178, %struct.ScmObj* %_37foldr47096, i64 2)
%argslist53286$k474130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54544 = alloca %struct.ScmObj*, align 8
%argslist53286$k474131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48178, %struct.ScmObj* %argslist53286$k474130)
store volatile %struct.ScmObj* %argslist53286$k474131, %struct.ScmObj** %stackaddr$prim54544, align 8
%stackaddr$prim54545 = alloca %struct.ScmObj*, align 8
%argslist53286$k474132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48177, %struct.ScmObj* %argslist53286$k474131)
store volatile %struct.ScmObj* %argslist53286$k474132, %struct.ScmObj** %stackaddr$prim54545, align 8
%clofunc54546 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47413)
musttail call tailcc void %clofunc54546(%struct.ScmObj* %k47413, %struct.ScmObj* %argslist53286$k474132)
ret void
}

define tailcc void @proc_clo$ae48178(%struct.ScmObj* %env$ae48178,%struct.ScmObj* %args4709747414) {
%stackaddr$env-ref54547 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48178, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54547
%stackaddr$env-ref54548 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48178, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54548
%stackaddr$env-ref54549 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48178, i64 2)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54549
%stackaddr$prim54550 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4709747414)
store volatile %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$prim54550, align 8
%stackaddr$prim54551 = alloca %struct.ScmObj*, align 8
%args47097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4709747414)
store volatile %struct.ScmObj* %args47097, %struct.ScmObj** %stackaddr$prim54551, align 8
%stackaddr$prim54552 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47097)
store volatile %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$prim54552, align 8
%stackaddr$prim54553 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47097)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim54553, align 8
%stackaddr$prim54554 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47226)
store volatile %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$prim54554, align 8
%stackaddr$prim54555 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47097)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim54555, align 8
%stackaddr$prim54556 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47227)
store volatile %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$prim54556, align 8
%stackaddr$makeclosure54557 = alloca %struct.ScmObj*, align 8
%fptrToInt54558 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48186 to i64
%ae48186 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54558)
store volatile %struct.ScmObj* %ae48186, %struct.ScmObj** %stackaddr$makeclosure54557, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48186, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48186, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48186, %struct.ScmObj* %f47100, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48186, %struct.ScmObj* %acc47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48186, %struct.ScmObj* %lsts47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48186, %struct.ScmObj* %_37foldr47096, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48186, %struct.ScmObj* %k47415, i64 6)
%ae48187 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54559 = alloca %struct.ScmObj*, align 8
%fptrToInt54560 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48188 to i64
%ae48188 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54560)
store volatile %struct.ScmObj* %ae48188, %struct.ScmObj** %stackaddr$makeclosure54559, align 8
%argslist53285$ae481860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54561 = alloca %struct.ScmObj*, align 8
%argslist53285$ae481861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48188, %struct.ScmObj* %argslist53285$ae481860)
store volatile %struct.ScmObj* %argslist53285$ae481861, %struct.ScmObj** %stackaddr$prim54561, align 8
%stackaddr$prim54562 = alloca %struct.ScmObj*, align 8
%argslist53285$ae481862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48187, %struct.ScmObj* %argslist53285$ae481861)
store volatile %struct.ScmObj* %argslist53285$ae481862, %struct.ScmObj** %stackaddr$prim54562, align 8
%clofunc54563 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48186)
musttail call tailcc void %clofunc54563(%struct.ScmObj* %ae48186, %struct.ScmObj* %argslist53285$ae481862)
ret void
}

define tailcc void @proc_clo$ae48186(%struct.ScmObj* %env$ae48186,%struct.ScmObj* %current_45args53231) {
%stackaddr$env-ref54564 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48186, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54564
%stackaddr$env-ref54565 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48186, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54565
%stackaddr$env-ref54566 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48186, i64 2)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54566
%stackaddr$env-ref54567 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48186, i64 3)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54567
%stackaddr$env-ref54568 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48186, i64 4)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref54568
%stackaddr$env-ref54569 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48186, i64 5)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54569
%stackaddr$env-ref54570 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48186, i64 6)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref54570
%stackaddr$prim54571 = alloca %struct.ScmObj*, align 8
%_95k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53231)
store volatile %struct.ScmObj* %_95k47416, %struct.ScmObj** %stackaddr$prim54571, align 8
%stackaddr$prim54572 = alloca %struct.ScmObj*, align 8
%current_45args53232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53231)
store volatile %struct.ScmObj* %current_45args53232, %struct.ScmObj** %stackaddr$prim54572, align 8
%stackaddr$prim54573 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53232)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim54573, align 8
%stackaddr$makeclosure54574 = alloca %struct.ScmObj*, align 8
%fptrToInt54575 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48218 to i64
%ae48218 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54575)
store volatile %struct.ScmObj* %ae48218, %struct.ScmObj** %stackaddr$makeclosure54574, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48218, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48218, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48218, %struct.ScmObj* %f47100, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48218, %struct.ScmObj* %acc47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48218, %struct.ScmObj* %lsts47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48218, %struct.ScmObj* %_37foldr47096, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48218, %struct.ScmObj* %k47415, i64 6)
%ae48220 = call %struct.ScmObj* @const_init_false()
%argslist53278$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54576 = alloca %struct.ScmObj*, align 8
%argslist53278$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47098, %struct.ScmObj* %argslist53278$_37foldr1470900)
store volatile %struct.ScmObj* %argslist53278$_37foldr1470901, %struct.ScmObj** %stackaddr$prim54576, align 8
%stackaddr$prim54577 = alloca %struct.ScmObj*, align 8
%argslist53278$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48220, %struct.ScmObj* %argslist53278$_37foldr1470901)
store volatile %struct.ScmObj* %argslist53278$_37foldr1470902, %struct.ScmObj** %stackaddr$prim54577, align 8
%stackaddr$prim54578 = alloca %struct.ScmObj*, align 8
%argslist53278$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47228, %struct.ScmObj* %argslist53278$_37foldr1470902)
store volatile %struct.ScmObj* %argslist53278$_37foldr1470903, %struct.ScmObj** %stackaddr$prim54578, align 8
%stackaddr$prim54579 = alloca %struct.ScmObj*, align 8
%argslist53278$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48218, %struct.ScmObj* %argslist53278$_37foldr1470903)
store volatile %struct.ScmObj* %argslist53278$_37foldr1470904, %struct.ScmObj** %stackaddr$prim54579, align 8
%clofunc54580 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc54580(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist53278$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae48218(%struct.ScmObj* %env$ae48218,%struct.ScmObj* %current_45args53234) {
%stackaddr$env-ref54581 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48218, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54581
%stackaddr$env-ref54582 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48218, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54582
%stackaddr$env-ref54583 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48218, i64 2)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54583
%stackaddr$env-ref54584 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48218, i64 3)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54584
%stackaddr$env-ref54585 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48218, i64 4)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref54585
%stackaddr$env-ref54586 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48218, i64 5)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54586
%stackaddr$env-ref54587 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48218, i64 6)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref54587
%stackaddr$prim54588 = alloca %struct.ScmObj*, align 8
%_95k47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53234)
store volatile %struct.ScmObj* %_95k47417, %struct.ScmObj** %stackaddr$prim54588, align 8
%stackaddr$prim54589 = alloca %struct.ScmObj*, align 8
%current_45args53235 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53234)
store volatile %struct.ScmObj* %current_45args53235, %struct.ScmObj** %stackaddr$prim54589, align 8
%stackaddr$prim54590 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53235)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim54590, align 8
%truthy$cmp54591 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47229)
%cmp$cmp54591 = icmp eq i64 %truthy$cmp54591, 1
br i1 %cmp$cmp54591, label %truebranch$cmp54591, label %falsebranch$cmp54591
truebranch$cmp54591:
%ae48229 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53237$k474150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54592 = alloca %struct.ScmObj*, align 8
%argslist53237$k474151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47099, %struct.ScmObj* %argslist53237$k474150)
store volatile %struct.ScmObj* %argslist53237$k474151, %struct.ScmObj** %stackaddr$prim54592, align 8
%stackaddr$prim54593 = alloca %struct.ScmObj*, align 8
%argslist53237$k474152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48229, %struct.ScmObj* %argslist53237$k474151)
store volatile %struct.ScmObj* %argslist53237$k474152, %struct.ScmObj** %stackaddr$prim54593, align 8
%clofunc54594 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47415)
musttail call tailcc void %clofunc54594(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist53237$k474152)
ret void
falsebranch$cmp54591:
%stackaddr$makeclosure54595 = alloca %struct.ScmObj*, align 8
%fptrToInt54596 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48234 to i64
%ae48234 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54596)
store volatile %struct.ScmObj* %ae48234, %struct.ScmObj** %stackaddr$makeclosure54595, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %f47100, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %acc47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %lsts47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %_37foldr47096, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %k47415, i64 6)
%ae48235 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54597 = alloca %struct.ScmObj*, align 8
%fptrToInt54598 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48236 to i64
%ae48236 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54598)
store volatile %struct.ScmObj* %ae48236, %struct.ScmObj** %stackaddr$makeclosure54597, align 8
%argslist53277$ae482340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54599 = alloca %struct.ScmObj*, align 8
%argslist53277$ae482341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48236, %struct.ScmObj* %argslist53277$ae482340)
store volatile %struct.ScmObj* %argslist53277$ae482341, %struct.ScmObj** %stackaddr$prim54599, align 8
%stackaddr$prim54600 = alloca %struct.ScmObj*, align 8
%argslist53277$ae482342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48235, %struct.ScmObj* %argslist53277$ae482341)
store volatile %struct.ScmObj* %argslist53277$ae482342, %struct.ScmObj** %stackaddr$prim54600, align 8
%clofunc54601 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48234)
musttail call tailcc void %clofunc54601(%struct.ScmObj* %ae48234, %struct.ScmObj* %argslist53277$ae482342)
ret void
}

define tailcc void @proc_clo$ae48234(%struct.ScmObj* %env$ae48234,%struct.ScmObj* %current_45args53238) {
%stackaddr$env-ref54602 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54602
%stackaddr$env-ref54603 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54603
%stackaddr$env-ref54604 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 2)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54604
%stackaddr$env-ref54605 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 3)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54605
%stackaddr$env-ref54606 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 4)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref54606
%stackaddr$env-ref54607 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 5)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54607
%stackaddr$env-ref54608 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 6)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref54608
%stackaddr$prim54609 = alloca %struct.ScmObj*, align 8
%_95k47418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53238)
store volatile %struct.ScmObj* %_95k47418, %struct.ScmObj** %stackaddr$prim54609, align 8
%stackaddr$prim54610 = alloca %struct.ScmObj*, align 8
%current_45args53239 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53238)
store volatile %struct.ScmObj* %current_45args53239, %struct.ScmObj** %stackaddr$prim54610, align 8
%stackaddr$prim54611 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53239)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim54611, align 8
%stackaddr$makeclosure54612 = alloca %struct.ScmObj*, align 8
%fptrToInt54613 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48255 to i64
%ae48255 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54613)
store volatile %struct.ScmObj* %ae48255, %struct.ScmObj** %stackaddr$makeclosure54612, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %f47100, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %acc47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %lsts47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %_37foldr47096, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %k47415, i64 6)
%argslist53272$_37map1470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54614 = alloca %struct.ScmObj*, align 8
%argslist53272$_37map1470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47098, %struct.ScmObj* %argslist53272$_37map1470860)
store volatile %struct.ScmObj* %argslist53272$_37map1470861, %struct.ScmObj** %stackaddr$prim54614, align 8
%stackaddr$prim54615 = alloca %struct.ScmObj*, align 8
%argslist53272$_37map1470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47230, %struct.ScmObj* %argslist53272$_37map1470861)
store volatile %struct.ScmObj* %argslist53272$_37map1470862, %struct.ScmObj** %stackaddr$prim54615, align 8
%stackaddr$prim54616 = alloca %struct.ScmObj*, align 8
%argslist53272$_37map1470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48255, %struct.ScmObj* %argslist53272$_37map1470862)
store volatile %struct.ScmObj* %argslist53272$_37map1470863, %struct.ScmObj** %stackaddr$prim54616, align 8
%clofunc54617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147086)
musttail call tailcc void %clofunc54617(%struct.ScmObj* %_37map147086, %struct.ScmObj* %argslist53272$_37map1470863)
ret void
}

define tailcc void @proc_clo$ae48255(%struct.ScmObj* %env$ae48255,%struct.ScmObj* %current_45args53241) {
%stackaddr$env-ref54618 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54618
%stackaddr$env-ref54619 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54619
%stackaddr$env-ref54620 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 2)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54620
%stackaddr$env-ref54621 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 3)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54621
%stackaddr$env-ref54622 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 4)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref54622
%stackaddr$env-ref54623 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 5)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54623
%stackaddr$env-ref54624 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 6)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref54624
%stackaddr$prim54625 = alloca %struct.ScmObj*, align 8
%_95k47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53241)
store volatile %struct.ScmObj* %_95k47419, %struct.ScmObj** %stackaddr$prim54625, align 8
%stackaddr$prim54626 = alloca %struct.ScmObj*, align 8
%current_45args53242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53241)
store volatile %struct.ScmObj* %current_45args53242, %struct.ScmObj** %stackaddr$prim54626, align 8
%stackaddr$prim54627 = alloca %struct.ScmObj*, align 8
%lsts_4347105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53242)
store volatile %struct.ScmObj* %lsts_4347105, %struct.ScmObj** %stackaddr$prim54627, align 8
%stackaddr$makeclosure54628 = alloca %struct.ScmObj*, align 8
%fptrToInt54629 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48258 to i64
%ae48258 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54629)
store volatile %struct.ScmObj* %ae48258, %struct.ScmObj** %stackaddr$makeclosure54628, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %lsts_4347105, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %acc47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %lsts47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %_37foldr47096, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %k47415, i64 7)
%ae48259 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54630 = alloca %struct.ScmObj*, align 8
%fptrToInt54631 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48260 to i64
%ae48260 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54631)
store volatile %struct.ScmObj* %ae48260, %struct.ScmObj** %stackaddr$makeclosure54630, align 8
%argslist53271$ae482580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54632 = alloca %struct.ScmObj*, align 8
%argslist53271$ae482581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48260, %struct.ScmObj* %argslist53271$ae482580)
store volatile %struct.ScmObj* %argslist53271$ae482581, %struct.ScmObj** %stackaddr$prim54632, align 8
%stackaddr$prim54633 = alloca %struct.ScmObj*, align 8
%argslist53271$ae482582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48259, %struct.ScmObj* %argslist53271$ae482581)
store volatile %struct.ScmObj* %argslist53271$ae482582, %struct.ScmObj** %stackaddr$prim54633, align 8
%clofunc54634 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48258)
musttail call tailcc void %clofunc54634(%struct.ScmObj* %ae48258, %struct.ScmObj* %argslist53271$ae482582)
ret void
}

define tailcc void @proc_clo$ae48258(%struct.ScmObj* %env$ae48258,%struct.ScmObj* %current_45args53244) {
%stackaddr$env-ref54635 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54635
%stackaddr$env-ref54636 = alloca %struct.ScmObj*, align 8
%lsts_4347105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 1)
store %struct.ScmObj* %lsts_4347105, %struct.ScmObj** %stackaddr$env-ref54636
%stackaddr$env-ref54637 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54637
%stackaddr$env-ref54638 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54638
%stackaddr$env-ref54639 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 4)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54639
%stackaddr$env-ref54640 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 5)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref54640
%stackaddr$env-ref54641 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 6)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54641
%stackaddr$env-ref54642 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 7)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref54642
%stackaddr$prim54643 = alloca %struct.ScmObj*, align 8
%_95k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53244)
store volatile %struct.ScmObj* %_95k47420, %struct.ScmObj** %stackaddr$prim54643, align 8
%stackaddr$prim54644 = alloca %struct.ScmObj*, align 8
%current_45args53245 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53244)
store volatile %struct.ScmObj* %current_45args53245, %struct.ScmObj** %stackaddr$prim54644, align 8
%stackaddr$prim54645 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53245)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim54645, align 8
%stackaddr$makeclosure54646 = alloca %struct.ScmObj*, align 8
%fptrToInt54647 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48279 to i64
%ae48279 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54647)
store volatile %struct.ScmObj* %ae48279, %struct.ScmObj** %stackaddr$makeclosure54646, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %lsts_4347105, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %f47100, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %acc47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %_37foldr47096, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %k47415, i64 5)
%argslist53266$_37map1470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54648 = alloca %struct.ScmObj*, align 8
%argslist53266$_37map1470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47098, %struct.ScmObj* %argslist53266$_37map1470860)
store volatile %struct.ScmObj* %argslist53266$_37map1470861, %struct.ScmObj** %stackaddr$prim54648, align 8
%stackaddr$prim54649 = alloca %struct.ScmObj*, align 8
%argslist53266$_37map1470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47231, %struct.ScmObj* %argslist53266$_37map1470861)
store volatile %struct.ScmObj* %argslist53266$_37map1470862, %struct.ScmObj** %stackaddr$prim54649, align 8
%stackaddr$prim54650 = alloca %struct.ScmObj*, align 8
%argslist53266$_37map1470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48279, %struct.ScmObj* %argslist53266$_37map1470862)
store volatile %struct.ScmObj* %argslist53266$_37map1470863, %struct.ScmObj** %stackaddr$prim54650, align 8
%clofunc54651 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147086)
musttail call tailcc void %clofunc54651(%struct.ScmObj* %_37map147086, %struct.ScmObj* %argslist53266$_37map1470863)
ret void
}

define tailcc void @proc_clo$ae48279(%struct.ScmObj* %env$ae48279,%struct.ScmObj* %current_45args53247) {
%stackaddr$env-ref54652 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54652
%stackaddr$env-ref54653 = alloca %struct.ScmObj*, align 8
%lsts_4347105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 1)
store %struct.ScmObj* %lsts_4347105, %struct.ScmObj** %stackaddr$env-ref54653
%stackaddr$env-ref54654 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 2)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54654
%stackaddr$env-ref54655 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 3)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54655
%stackaddr$env-ref54656 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 4)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54656
%stackaddr$env-ref54657 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 5)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref54657
%stackaddr$prim54658 = alloca %struct.ScmObj*, align 8
%_95k47421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53247)
store volatile %struct.ScmObj* %_95k47421, %struct.ScmObj** %stackaddr$prim54658, align 8
%stackaddr$prim54659 = alloca %struct.ScmObj*, align 8
%current_45args53248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53247)
store volatile %struct.ScmObj* %current_45args53248, %struct.ScmObj** %stackaddr$prim54659, align 8
%stackaddr$prim54660 = alloca %struct.ScmObj*, align 8
%vs47103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53248)
store volatile %struct.ScmObj* %vs47103, %struct.ScmObj** %stackaddr$prim54660, align 8
%stackaddr$makeclosure54661 = alloca %struct.ScmObj*, align 8
%fptrToInt54662 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48282 to i64
%ae48282 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54662)
store volatile %struct.ScmObj* %ae48282, %struct.ScmObj** %stackaddr$makeclosure54661, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %lsts_4347105, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %vs47103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %acc47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %_37foldr47096, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %k47415, i64 6)
%ae48283 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54663 = alloca %struct.ScmObj*, align 8
%fptrToInt54664 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48284 to i64
%ae48284 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54664)
store volatile %struct.ScmObj* %ae48284, %struct.ScmObj** %stackaddr$makeclosure54663, align 8
%argslist53265$ae482820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54665 = alloca %struct.ScmObj*, align 8
%argslist53265$ae482821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48284, %struct.ScmObj* %argslist53265$ae482820)
store volatile %struct.ScmObj* %argslist53265$ae482821, %struct.ScmObj** %stackaddr$prim54665, align 8
%stackaddr$prim54666 = alloca %struct.ScmObj*, align 8
%argslist53265$ae482822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48283, %struct.ScmObj* %argslist53265$ae482821)
store volatile %struct.ScmObj* %argslist53265$ae482822, %struct.ScmObj** %stackaddr$prim54666, align 8
%clofunc54667 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48282)
musttail call tailcc void %clofunc54667(%struct.ScmObj* %ae48282, %struct.ScmObj* %argslist53265$ae482822)
ret void
}

define tailcc void @proc_clo$ae48282(%struct.ScmObj* %env$ae48282,%struct.ScmObj* %current_45args53250) {
%stackaddr$env-ref54668 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54668
%stackaddr$env-ref54669 = alloca %struct.ScmObj*, align 8
%lsts_4347105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 1)
store %struct.ScmObj* %lsts_4347105, %struct.ScmObj** %stackaddr$env-ref54669
%stackaddr$env-ref54670 = alloca %struct.ScmObj*, align 8
%vs47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 2)
store %struct.ScmObj* %vs47103, %struct.ScmObj** %stackaddr$env-ref54670
%stackaddr$env-ref54671 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54671
%stackaddr$env-ref54672 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 4)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54672
%stackaddr$env-ref54673 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 5)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54673
%stackaddr$env-ref54674 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 6)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref54674
%stackaddr$prim54675 = alloca %struct.ScmObj*, align 8
%_95k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53250)
store volatile %struct.ScmObj* %_95k47422, %struct.ScmObj** %stackaddr$prim54675, align 8
%stackaddr$prim54676 = alloca %struct.ScmObj*, align 8
%current_45args53251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53250)
store volatile %struct.ScmObj* %current_45args53251, %struct.ScmObj** %stackaddr$prim54676, align 8
%stackaddr$prim54677 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53251)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim54677, align 8
%stackaddr$prim54678 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47099, %struct.ScmObj* %lsts_4347105)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim54678, align 8
%stackaddr$prim54679 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47100, %struct.ScmObj* %anf_45bind47233)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim54679, align 8
%stackaddr$makeclosure54680 = alloca %struct.ScmObj*, align 8
%fptrToInt54681 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48308 to i64
%ae48308 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54681)
store volatile %struct.ScmObj* %ae48308, %struct.ScmObj** %stackaddr$makeclosure54680, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48308, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48308, %struct.ScmObj* %anf_45bind47232, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48308, %struct.ScmObj* %vs47103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48308, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48308, %struct.ScmObj* %k47415, i64 4)
%stackaddr$prim54682 = alloca %struct.ScmObj*, align 8
%cpsargs47426 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48308, %struct.ScmObj* %anf_45bind47234)
store volatile %struct.ScmObj* %cpsargs47426, %struct.ScmObj** %stackaddr$prim54682, align 8
%clofunc54683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47096)
musttail call tailcc void %clofunc54683(%struct.ScmObj* %_37foldr47096, %struct.ScmObj* %cpsargs47426)
ret void
}

define tailcc void @proc_clo$ae48308(%struct.ScmObj* %env$ae48308,%struct.ScmObj* %current_45args53253) {
%stackaddr$env-ref54684 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48308, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54684
%stackaddr$env-ref54685 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48308, i64 1)
store %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$env-ref54685
%stackaddr$env-ref54686 = alloca %struct.ScmObj*, align 8
%vs47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48308, i64 2)
store %struct.ScmObj* %vs47103, %struct.ScmObj** %stackaddr$env-ref54686
%stackaddr$env-ref54687 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48308, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54687
%stackaddr$env-ref54688 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48308, i64 4)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref54688
%stackaddr$prim54689 = alloca %struct.ScmObj*, align 8
%_95k47423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53253)
store volatile %struct.ScmObj* %_95k47423, %struct.ScmObj** %stackaddr$prim54689, align 8
%stackaddr$prim54690 = alloca %struct.ScmObj*, align 8
%current_45args53254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53253)
store volatile %struct.ScmObj* %current_45args53254, %struct.ScmObj** %stackaddr$prim54690, align 8
%stackaddr$prim54691 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53254)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim54691, align 8
%ae48313 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54692 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47235, %struct.ScmObj* %ae48313)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim54692, align 8
%stackaddr$makeclosure54693 = alloca %struct.ScmObj*, align 8
%fptrToInt54694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48315 to i64
%ae48315 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54694)
store volatile %struct.ScmObj* %ae48315, %struct.ScmObj** %stackaddr$makeclosure54693, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %f47100, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %k47415, i64 1)
%argslist53259$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54695 = alloca %struct.ScmObj*, align 8
%argslist53259$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47103, %struct.ScmObj* %argslist53259$_37foldr1470900)
store volatile %struct.ScmObj* %argslist53259$_37foldr1470901, %struct.ScmObj** %stackaddr$prim54695, align 8
%stackaddr$prim54696 = alloca %struct.ScmObj*, align 8
%argslist53259$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47236, %struct.ScmObj* %argslist53259$_37foldr1470901)
store volatile %struct.ScmObj* %argslist53259$_37foldr1470902, %struct.ScmObj** %stackaddr$prim54696, align 8
%stackaddr$prim54697 = alloca %struct.ScmObj*, align 8
%argslist53259$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47232, %struct.ScmObj* %argslist53259$_37foldr1470902)
store volatile %struct.ScmObj* %argslist53259$_37foldr1470903, %struct.ScmObj** %stackaddr$prim54697, align 8
%stackaddr$prim54698 = alloca %struct.ScmObj*, align 8
%argslist53259$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48315, %struct.ScmObj* %argslist53259$_37foldr1470903)
store volatile %struct.ScmObj* %argslist53259$_37foldr1470904, %struct.ScmObj** %stackaddr$prim54698, align 8
%clofunc54699 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc54699(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist53259$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae48315(%struct.ScmObj* %env$ae48315,%struct.ScmObj* %current_45args53256) {
%stackaddr$env-ref54700 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 0)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54700
%stackaddr$env-ref54701 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 1)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref54701
%stackaddr$prim54702 = alloca %struct.ScmObj*, align 8
%_95k47424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53256)
store volatile %struct.ScmObj* %_95k47424, %struct.ScmObj** %stackaddr$prim54702, align 8
%stackaddr$prim54703 = alloca %struct.ScmObj*, align 8
%current_45args53257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53256)
store volatile %struct.ScmObj* %current_45args53257, %struct.ScmObj** %stackaddr$prim54703, align 8
%stackaddr$prim54704 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53257)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim54704, align 8
%stackaddr$prim54705 = alloca %struct.ScmObj*, align 8
%cpsargs47425 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47415, %struct.ScmObj* %anf_45bind47237)
store volatile %struct.ScmObj* %cpsargs47425, %struct.ScmObj** %stackaddr$prim54705, align 8
%clofunc54706 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47100)
musttail call tailcc void %clofunc54706(%struct.ScmObj* %f47100, %struct.ScmObj* %cpsargs47425)
ret void
}

define tailcc void @proc_clo$ae48284(%struct.ScmObj* %env$ae48284,%struct.ScmObj* %current_45args53260) {
%stackaddr$prim54707 = alloca %struct.ScmObj*, align 8
%k47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53260)
store volatile %struct.ScmObj* %k47427, %struct.ScmObj** %stackaddr$prim54707, align 8
%stackaddr$prim54708 = alloca %struct.ScmObj*, align 8
%current_45args53261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53260)
store volatile %struct.ScmObj* %current_45args53261, %struct.ScmObj** %stackaddr$prim54708, align 8
%stackaddr$prim54709 = alloca %struct.ScmObj*, align 8
%a47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53261)
store volatile %struct.ScmObj* %a47108, %struct.ScmObj** %stackaddr$prim54709, align 8
%stackaddr$prim54710 = alloca %struct.ScmObj*, align 8
%current_45args53262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53261)
store volatile %struct.ScmObj* %current_45args53262, %struct.ScmObj** %stackaddr$prim54710, align 8
%stackaddr$prim54711 = alloca %struct.ScmObj*, align 8
%b47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53262)
store volatile %struct.ScmObj* %b47107, %struct.ScmObj** %stackaddr$prim54711, align 8
%stackaddr$prim54712 = alloca %struct.ScmObj*, align 8
%cpsprim47428 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47108, %struct.ScmObj* %b47107)
store volatile %struct.ScmObj* %cpsprim47428, %struct.ScmObj** %stackaddr$prim54712, align 8
%ae48288 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53264$k474270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54713 = alloca %struct.ScmObj*, align 8
%argslist53264$k474271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47428, %struct.ScmObj* %argslist53264$k474270)
store volatile %struct.ScmObj* %argslist53264$k474271, %struct.ScmObj** %stackaddr$prim54713, align 8
%stackaddr$prim54714 = alloca %struct.ScmObj*, align 8
%argslist53264$k474272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48288, %struct.ScmObj* %argslist53264$k474271)
store volatile %struct.ScmObj* %argslist53264$k474272, %struct.ScmObj** %stackaddr$prim54714, align 8
%clofunc54715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47427)
musttail call tailcc void %clofunc54715(%struct.ScmObj* %k47427, %struct.ScmObj* %argslist53264$k474272)
ret void
}

define tailcc void @proc_clo$ae48260(%struct.ScmObj* %env$ae48260,%struct.ScmObj* %current_45args53267) {
%stackaddr$prim54716 = alloca %struct.ScmObj*, align 8
%k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53267)
store volatile %struct.ScmObj* %k47429, %struct.ScmObj** %stackaddr$prim54716, align 8
%stackaddr$prim54717 = alloca %struct.ScmObj*, align 8
%current_45args53268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53267)
store volatile %struct.ScmObj* %current_45args53268, %struct.ScmObj** %stackaddr$prim54717, align 8
%stackaddr$prim54718 = alloca %struct.ScmObj*, align 8
%x47104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53268)
store volatile %struct.ScmObj* %x47104, %struct.ScmObj** %stackaddr$prim54718, align 8
%stackaddr$prim54719 = alloca %struct.ScmObj*, align 8
%cpsprim47430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47104)
store volatile %struct.ScmObj* %cpsprim47430, %struct.ScmObj** %stackaddr$prim54719, align 8
%ae48263 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53270$k474290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54720 = alloca %struct.ScmObj*, align 8
%argslist53270$k474291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47430, %struct.ScmObj* %argslist53270$k474290)
store volatile %struct.ScmObj* %argslist53270$k474291, %struct.ScmObj** %stackaddr$prim54720, align 8
%stackaddr$prim54721 = alloca %struct.ScmObj*, align 8
%argslist53270$k474292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48263, %struct.ScmObj* %argslist53270$k474291)
store volatile %struct.ScmObj* %argslist53270$k474292, %struct.ScmObj** %stackaddr$prim54721, align 8
%clofunc54722 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47429)
musttail call tailcc void %clofunc54722(%struct.ScmObj* %k47429, %struct.ScmObj* %argslist53270$k474292)
ret void
}

define tailcc void @proc_clo$ae48236(%struct.ScmObj* %env$ae48236,%struct.ScmObj* %current_45args53273) {
%stackaddr$prim54723 = alloca %struct.ScmObj*, align 8
%k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53273)
store volatile %struct.ScmObj* %k47431, %struct.ScmObj** %stackaddr$prim54723, align 8
%stackaddr$prim54724 = alloca %struct.ScmObj*, align 8
%current_45args53274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53273)
store volatile %struct.ScmObj* %current_45args53274, %struct.ScmObj** %stackaddr$prim54724, align 8
%stackaddr$prim54725 = alloca %struct.ScmObj*, align 8
%x47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53274)
store volatile %struct.ScmObj* %x47106, %struct.ScmObj** %stackaddr$prim54725, align 8
%stackaddr$prim54726 = alloca %struct.ScmObj*, align 8
%cpsprim47432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47106)
store volatile %struct.ScmObj* %cpsprim47432, %struct.ScmObj** %stackaddr$prim54726, align 8
%ae48239 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53276$k474310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54727 = alloca %struct.ScmObj*, align 8
%argslist53276$k474311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47432, %struct.ScmObj* %argslist53276$k474310)
store volatile %struct.ScmObj* %argslist53276$k474311, %struct.ScmObj** %stackaddr$prim54727, align 8
%stackaddr$prim54728 = alloca %struct.ScmObj*, align 8
%argslist53276$k474312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48239, %struct.ScmObj* %argslist53276$k474311)
store volatile %struct.ScmObj* %argslist53276$k474312, %struct.ScmObj** %stackaddr$prim54728, align 8
%clofunc54729 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47431)
musttail call tailcc void %clofunc54729(%struct.ScmObj* %k47431, %struct.ScmObj* %argslist53276$k474312)
ret void
}

define tailcc void @proc_clo$ae48188(%struct.ScmObj* %env$ae48188,%struct.ScmObj* %current_45args53279) {
%stackaddr$prim54730 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53279)
store volatile %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$prim54730, align 8
%stackaddr$prim54731 = alloca %struct.ScmObj*, align 8
%current_45args53280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53279)
store volatile %struct.ScmObj* %current_45args53280, %struct.ScmObj** %stackaddr$prim54731, align 8
%stackaddr$prim54732 = alloca %struct.ScmObj*, align 8
%lst47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53280)
store volatile %struct.ScmObj* %lst47102, %struct.ScmObj** %stackaddr$prim54732, align 8
%stackaddr$prim54733 = alloca %struct.ScmObj*, align 8
%current_45args53281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53280)
store volatile %struct.ScmObj* %current_45args53281, %struct.ScmObj** %stackaddr$prim54733, align 8
%stackaddr$prim54734 = alloca %struct.ScmObj*, align 8
%b47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53281)
store volatile %struct.ScmObj* %b47101, %struct.ScmObj** %stackaddr$prim54734, align 8
%truthy$cmp54735 = call i64 @is_truthy_value(%struct.ScmObj* %b47101)
%cmp$cmp54735 = icmp eq i64 %truthy$cmp54735, 1
br i1 %cmp$cmp54735, label %truebranch$cmp54735, label %falsebranch$cmp54735
truebranch$cmp54735:
%ae48191 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53283$k474330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54736 = alloca %struct.ScmObj*, align 8
%argslist53283$k474331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47101, %struct.ScmObj* %argslist53283$k474330)
store volatile %struct.ScmObj* %argslist53283$k474331, %struct.ScmObj** %stackaddr$prim54736, align 8
%stackaddr$prim54737 = alloca %struct.ScmObj*, align 8
%argslist53283$k474332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48191, %struct.ScmObj* %argslist53283$k474331)
store volatile %struct.ScmObj* %argslist53283$k474332, %struct.ScmObj** %stackaddr$prim54737, align 8
%clofunc54738 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47433)
musttail call tailcc void %clofunc54738(%struct.ScmObj* %k47433, %struct.ScmObj* %argslist53283$k474332)
ret void
falsebranch$cmp54735:
%stackaddr$prim54739 = alloca %struct.ScmObj*, align 8
%cpsprim47434 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47102)
store volatile %struct.ScmObj* %cpsprim47434, %struct.ScmObj** %stackaddr$prim54739, align 8
%ae48198 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53284$k474330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54740 = alloca %struct.ScmObj*, align 8
%argslist53284$k474331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47434, %struct.ScmObj* %argslist53284$k474330)
store volatile %struct.ScmObj* %argslist53284$k474331, %struct.ScmObj** %stackaddr$prim54740, align 8
%stackaddr$prim54741 = alloca %struct.ScmObj*, align 8
%argslist53284$k474332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48198, %struct.ScmObj* %argslist53284$k474331)
store volatile %struct.ScmObj* %argslist53284$k474332, %struct.ScmObj** %stackaddr$prim54741, align 8
%clofunc54742 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47433)
musttail call tailcc void %clofunc54742(%struct.ScmObj* %k47433, %struct.ScmObj* %argslist53284$k474332)
ret void
}

define tailcc void @proc_clo$ae48145(%struct.ScmObj* %env$ae48145,%struct.ScmObj* %current_45args53288) {
%stackaddr$env-ref54743 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48145, i64 0)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref54743
%stackaddr$env-ref54744 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48145, i64 1)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref54744
%stackaddr$prim54745 = alloca %struct.ScmObj*, align 8
%k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53288)
store volatile %struct.ScmObj* %k47435, %struct.ScmObj** %stackaddr$prim54745, align 8
%stackaddr$prim54746 = alloca %struct.ScmObj*, align 8
%current_45args53289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53288)
store volatile %struct.ScmObj* %current_45args53289, %struct.ScmObj** %stackaddr$prim54746, align 8
%stackaddr$prim54747 = alloca %struct.ScmObj*, align 8
%lst47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53289)
store volatile %struct.ScmObj* %lst47111, %struct.ScmObj** %stackaddr$prim54747, align 8
%stackaddr$prim54748 = alloca %struct.ScmObj*, align 8
%current_45args53290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53289)
store volatile %struct.ScmObj* %current_45args53290, %struct.ScmObj** %stackaddr$prim54748, align 8
%stackaddr$prim54749 = alloca %struct.ScmObj*, align 8
%n47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53290)
store volatile %struct.ScmObj* %n47110, %struct.ScmObj** %stackaddr$prim54749, align 8
%stackaddr$makeclosure54750 = alloca %struct.ScmObj*, align 8
%fptrToInt54751 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48147 to i64
%ae48147 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54751)
store volatile %struct.ScmObj* %ae48147, %struct.ScmObj** %stackaddr$makeclosure54750, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48147, %struct.ScmObj* %n47110, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48147, %struct.ScmObj* %k47435, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48147, %struct.ScmObj* %_37take47082, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48147, %struct.ScmObj* %lst47111, i64 3)
%argslist53296$_37length470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54752 = alloca %struct.ScmObj*, align 8
%argslist53296$_37length470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47111, %struct.ScmObj* %argslist53296$_37length470790)
store volatile %struct.ScmObj* %argslist53296$_37length470791, %struct.ScmObj** %stackaddr$prim54752, align 8
%stackaddr$prim54753 = alloca %struct.ScmObj*, align 8
%argslist53296$_37length470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48147, %struct.ScmObj* %argslist53296$_37length470791)
store volatile %struct.ScmObj* %argslist53296$_37length470792, %struct.ScmObj** %stackaddr$prim54753, align 8
%clofunc54754 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47079)
musttail call tailcc void %clofunc54754(%struct.ScmObj* %_37length47079, %struct.ScmObj* %argslist53296$_37length470792)
ret void
}

define tailcc void @proc_clo$ae48147(%struct.ScmObj* %env$ae48147,%struct.ScmObj* %current_45args53292) {
%stackaddr$env-ref54755 = alloca %struct.ScmObj*, align 8
%n47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48147, i64 0)
store %struct.ScmObj* %n47110, %struct.ScmObj** %stackaddr$env-ref54755
%stackaddr$env-ref54756 = alloca %struct.ScmObj*, align 8
%k47435 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48147, i64 1)
store %struct.ScmObj* %k47435, %struct.ScmObj** %stackaddr$env-ref54756
%stackaddr$env-ref54757 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48147, i64 2)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref54757
%stackaddr$env-ref54758 = alloca %struct.ScmObj*, align 8
%lst47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48147, i64 3)
store %struct.ScmObj* %lst47111, %struct.ScmObj** %stackaddr$env-ref54758
%stackaddr$prim54759 = alloca %struct.ScmObj*, align 8
%_95k47436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53292)
store volatile %struct.ScmObj* %_95k47436, %struct.ScmObj** %stackaddr$prim54759, align 8
%stackaddr$prim54760 = alloca %struct.ScmObj*, align 8
%current_45args53293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53292)
store volatile %struct.ScmObj* %current_45args53293, %struct.ScmObj** %stackaddr$prim54760, align 8
%stackaddr$prim54761 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53293)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim54761, align 8
%stackaddr$prim54762 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47224, %struct.ScmObj* %n47110)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim54762, align 8
%argslist53295$_37take470820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54763 = alloca %struct.ScmObj*, align 8
%argslist53295$_37take470821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47225, %struct.ScmObj* %argslist53295$_37take470820)
store volatile %struct.ScmObj* %argslist53295$_37take470821, %struct.ScmObj** %stackaddr$prim54763, align 8
%stackaddr$prim54764 = alloca %struct.ScmObj*, align 8
%argslist53295$_37take470822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47111, %struct.ScmObj* %argslist53295$_37take470821)
store volatile %struct.ScmObj* %argslist53295$_37take470822, %struct.ScmObj** %stackaddr$prim54764, align 8
%stackaddr$prim54765 = alloca %struct.ScmObj*, align 8
%argslist53295$_37take470823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47435, %struct.ScmObj* %argslist53295$_37take470822)
store volatile %struct.ScmObj* %argslist53295$_37take470823, %struct.ScmObj** %stackaddr$prim54765, align 8
%clofunc54766 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47082)
musttail call tailcc void %clofunc54766(%struct.ScmObj* %_37take47082, %struct.ScmObj* %argslist53295$_37take470823)
ret void
}

define tailcc void @proc_clo$ae48091(%struct.ScmObj* %env$ae48091,%struct.ScmObj* %current_45args53298) {
%stackaddr$env-ref54767 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48091, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54767
%stackaddr$prim54768 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53298)
store volatile %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$prim54768, align 8
%stackaddr$prim54769 = alloca %struct.ScmObj*, align 8
%current_45args53299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53298)
store volatile %struct.ScmObj* %current_45args53299, %struct.ScmObj** %stackaddr$prim54769, align 8
%stackaddr$prim54770 = alloca %struct.ScmObj*, align 8
%lst47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53299)
store volatile %struct.ScmObj* %lst47113, %struct.ScmObj** %stackaddr$prim54770, align 8
%stackaddr$makeclosure54771 = alloca %struct.ScmObj*, align 8
%fptrToInt54772 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48092 to i64
%ae48092 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54772)
store volatile %struct.ScmObj* %ae48092, %struct.ScmObj** %stackaddr$makeclosure54771, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48092, %struct.ScmObj* %_37foldl147074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48092, %struct.ScmObj* %k47437, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48092, %struct.ScmObj* %lst47113, i64 2)
%ae48093 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54773 = alloca %struct.ScmObj*, align 8
%fptrToInt54774 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48094 to i64
%ae48094 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54774)
store volatile %struct.ScmObj* %ae48094, %struct.ScmObj** %stackaddr$makeclosure54773, align 8
%argslist53310$ae480920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54775 = alloca %struct.ScmObj*, align 8
%argslist53310$ae480921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48094, %struct.ScmObj* %argslist53310$ae480920)
store volatile %struct.ScmObj* %argslist53310$ae480921, %struct.ScmObj** %stackaddr$prim54775, align 8
%stackaddr$prim54776 = alloca %struct.ScmObj*, align 8
%argslist53310$ae480922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48093, %struct.ScmObj* %argslist53310$ae480921)
store volatile %struct.ScmObj* %argslist53310$ae480922, %struct.ScmObj** %stackaddr$prim54776, align 8
%clofunc54777 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48092)
musttail call tailcc void %clofunc54777(%struct.ScmObj* %ae48092, %struct.ScmObj* %argslist53310$ae480922)
ret void
}

define tailcc void @proc_clo$ae48092(%struct.ScmObj* %env$ae48092,%struct.ScmObj* %current_45args53301) {
%stackaddr$env-ref54778 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48092, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54778
%stackaddr$env-ref54779 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48092, i64 1)
store %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$env-ref54779
%stackaddr$env-ref54780 = alloca %struct.ScmObj*, align 8
%lst47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48092, i64 2)
store %struct.ScmObj* %lst47113, %struct.ScmObj** %stackaddr$env-ref54780
%stackaddr$prim54781 = alloca %struct.ScmObj*, align 8
%_95k47438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53301)
store volatile %struct.ScmObj* %_95k47438, %struct.ScmObj** %stackaddr$prim54781, align 8
%stackaddr$prim54782 = alloca %struct.ScmObj*, align 8
%current_45args53302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53301)
store volatile %struct.ScmObj* %current_45args53302, %struct.ScmObj** %stackaddr$prim54782, align 8
%stackaddr$prim54783 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53302)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim54783, align 8
%ae48113 = call %struct.ScmObj* @const_init_null()
%argslist53304$_37foldl1470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54784 = alloca %struct.ScmObj*, align 8
%argslist53304$_37foldl1470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47113, %struct.ScmObj* %argslist53304$_37foldl1470740)
store volatile %struct.ScmObj* %argslist53304$_37foldl1470741, %struct.ScmObj** %stackaddr$prim54784, align 8
%stackaddr$prim54785 = alloca %struct.ScmObj*, align 8
%argslist53304$_37foldl1470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48113, %struct.ScmObj* %argslist53304$_37foldl1470741)
store volatile %struct.ScmObj* %argslist53304$_37foldl1470742, %struct.ScmObj** %stackaddr$prim54785, align 8
%stackaddr$prim54786 = alloca %struct.ScmObj*, align 8
%argslist53304$_37foldl1470743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47223, %struct.ScmObj* %argslist53304$_37foldl1470742)
store volatile %struct.ScmObj* %argslist53304$_37foldl1470743, %struct.ScmObj** %stackaddr$prim54786, align 8
%stackaddr$prim54787 = alloca %struct.ScmObj*, align 8
%argslist53304$_37foldl1470744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47437, %struct.ScmObj* %argslist53304$_37foldl1470743)
store volatile %struct.ScmObj* %argslist53304$_37foldl1470744, %struct.ScmObj** %stackaddr$prim54787, align 8
%clofunc54788 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147074)
musttail call tailcc void %clofunc54788(%struct.ScmObj* %_37foldl147074, %struct.ScmObj* %argslist53304$_37foldl1470744)
ret void
}

define tailcc void @proc_clo$ae48094(%struct.ScmObj* %env$ae48094,%struct.ScmObj* %current_45args53305) {
%stackaddr$prim54789 = alloca %struct.ScmObj*, align 8
%k47439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53305)
store volatile %struct.ScmObj* %k47439, %struct.ScmObj** %stackaddr$prim54789, align 8
%stackaddr$prim54790 = alloca %struct.ScmObj*, align 8
%current_45args53306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53305)
store volatile %struct.ScmObj* %current_45args53306, %struct.ScmObj** %stackaddr$prim54790, align 8
%stackaddr$prim54791 = alloca %struct.ScmObj*, align 8
%x47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53306)
store volatile %struct.ScmObj* %x47115, %struct.ScmObj** %stackaddr$prim54791, align 8
%stackaddr$prim54792 = alloca %struct.ScmObj*, align 8
%current_45args53307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53306)
store volatile %struct.ScmObj* %current_45args53307, %struct.ScmObj** %stackaddr$prim54792, align 8
%stackaddr$prim54793 = alloca %struct.ScmObj*, align 8
%y47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53307)
store volatile %struct.ScmObj* %y47114, %struct.ScmObj** %stackaddr$prim54793, align 8
%ae48096 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53309$k474390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54794 = alloca %struct.ScmObj*, align 8
%argslist53309$k474391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47115, %struct.ScmObj* %argslist53309$k474390)
store volatile %struct.ScmObj* %argslist53309$k474391, %struct.ScmObj** %stackaddr$prim54794, align 8
%stackaddr$prim54795 = alloca %struct.ScmObj*, align 8
%argslist53309$k474392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48096, %struct.ScmObj* %argslist53309$k474391)
store volatile %struct.ScmObj* %argslist53309$k474392, %struct.ScmObj** %stackaddr$prim54795, align 8
%clofunc54796 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47439)
musttail call tailcc void %clofunc54796(%struct.ScmObj* %k47439, %struct.ScmObj* %argslist53309$k474392)
ret void
}

define tailcc void @proc_clo$ae48012(%struct.ScmObj* %env$ae48012,%struct.ScmObj* %current_45args53313) {
%stackaddr$prim54797 = alloca %struct.ScmObj*, align 8
%k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53313)
store volatile %struct.ScmObj* %k47440, %struct.ScmObj** %stackaddr$prim54797, align 8
%stackaddr$prim54798 = alloca %struct.ScmObj*, align 8
%current_45args53314 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53313)
store volatile %struct.ScmObj* %current_45args53314, %struct.ScmObj** %stackaddr$prim54798, align 8
%stackaddr$prim54799 = alloca %struct.ScmObj*, align 8
%_37foldl147075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53314)
store volatile %struct.ScmObj* %_37foldl147075, %struct.ScmObj** %stackaddr$prim54799, align 8
%ae48014 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54800 = alloca %struct.ScmObj*, align 8
%fptrToInt54801 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48015 to i64
%ae48015 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54801)
store volatile %struct.ScmObj* %ae48015, %struct.ScmObj** %stackaddr$makeclosure54800, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48015, %struct.ScmObj* %_37foldl147075, i64 0)
%argslist53327$k474400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54802 = alloca %struct.ScmObj*, align 8
%argslist53327$k474401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48015, %struct.ScmObj* %argslist53327$k474400)
store volatile %struct.ScmObj* %argslist53327$k474401, %struct.ScmObj** %stackaddr$prim54802, align 8
%stackaddr$prim54803 = alloca %struct.ScmObj*, align 8
%argslist53327$k474402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48014, %struct.ScmObj* %argslist53327$k474401)
store volatile %struct.ScmObj* %argslist53327$k474402, %struct.ScmObj** %stackaddr$prim54803, align 8
%clofunc54804 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47440)
musttail call tailcc void %clofunc54804(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist53327$k474402)
ret void
}

define tailcc void @proc_clo$ae48015(%struct.ScmObj* %env$ae48015,%struct.ScmObj* %current_45args53316) {
%stackaddr$env-ref54805 = alloca %struct.ScmObj*, align 8
%_37foldl147075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48015, i64 0)
store %struct.ScmObj* %_37foldl147075, %struct.ScmObj** %stackaddr$env-ref54805
%stackaddr$prim54806 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53316)
store volatile %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$prim54806, align 8
%stackaddr$prim54807 = alloca %struct.ScmObj*, align 8
%current_45args53317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53316)
store volatile %struct.ScmObj* %current_45args53317, %struct.ScmObj** %stackaddr$prim54807, align 8
%stackaddr$prim54808 = alloca %struct.ScmObj*, align 8
%f47078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53317)
store volatile %struct.ScmObj* %f47078, %struct.ScmObj** %stackaddr$prim54808, align 8
%stackaddr$prim54809 = alloca %struct.ScmObj*, align 8
%current_45args53318 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53317)
store volatile %struct.ScmObj* %current_45args53318, %struct.ScmObj** %stackaddr$prim54809, align 8
%stackaddr$prim54810 = alloca %struct.ScmObj*, align 8
%acc47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53318)
store volatile %struct.ScmObj* %acc47077, %struct.ScmObj** %stackaddr$prim54810, align 8
%stackaddr$prim54811 = alloca %struct.ScmObj*, align 8
%current_45args53319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53318)
store volatile %struct.ScmObj* %current_45args53319, %struct.ScmObj** %stackaddr$prim54811, align 8
%stackaddr$prim54812 = alloca %struct.ScmObj*, align 8
%lst47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53319)
store volatile %struct.ScmObj* %lst47076, %struct.ScmObj** %stackaddr$prim54812, align 8
%stackaddr$prim54813 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47076)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim54813, align 8
%truthy$cmp54814 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47218)
%cmp$cmp54814 = icmp eq i64 %truthy$cmp54814, 1
br i1 %cmp$cmp54814, label %truebranch$cmp54814, label %falsebranch$cmp54814
truebranch$cmp54814:
%ae48019 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53321$k474410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54815 = alloca %struct.ScmObj*, align 8
%argslist53321$k474411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47077, %struct.ScmObj* %argslist53321$k474410)
store volatile %struct.ScmObj* %argslist53321$k474411, %struct.ScmObj** %stackaddr$prim54815, align 8
%stackaddr$prim54816 = alloca %struct.ScmObj*, align 8
%argslist53321$k474412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48019, %struct.ScmObj* %argslist53321$k474411)
store volatile %struct.ScmObj* %argslist53321$k474412, %struct.ScmObj** %stackaddr$prim54816, align 8
%clofunc54817 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47441)
musttail call tailcc void %clofunc54817(%struct.ScmObj* %k47441, %struct.ScmObj* %argslist53321$k474412)
ret void
falsebranch$cmp54814:
%stackaddr$prim54818 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47076)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim54818, align 8
%stackaddr$makeclosure54819 = alloca %struct.ScmObj*, align 8
%fptrToInt54820 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48026 to i64
%ae48026 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54820)
store volatile %struct.ScmObj* %ae48026, %struct.ScmObj** %stackaddr$makeclosure54819, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48026, %struct.ScmObj* %f47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48026, %struct.ScmObj* %lst47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48026, %struct.ScmObj* %_37foldl147075, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48026, %struct.ScmObj* %k47441, i64 3)
%argslist53326$f470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54821 = alloca %struct.ScmObj*, align 8
%argslist53326$f470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47077, %struct.ScmObj* %argslist53326$f470780)
store volatile %struct.ScmObj* %argslist53326$f470781, %struct.ScmObj** %stackaddr$prim54821, align 8
%stackaddr$prim54822 = alloca %struct.ScmObj*, align 8
%argslist53326$f470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47219, %struct.ScmObj* %argslist53326$f470781)
store volatile %struct.ScmObj* %argslist53326$f470782, %struct.ScmObj** %stackaddr$prim54822, align 8
%stackaddr$prim54823 = alloca %struct.ScmObj*, align 8
%argslist53326$f470783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48026, %struct.ScmObj* %argslist53326$f470782)
store volatile %struct.ScmObj* %argslist53326$f470783, %struct.ScmObj** %stackaddr$prim54823, align 8
%clofunc54824 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47078)
musttail call tailcc void %clofunc54824(%struct.ScmObj* %f47078, %struct.ScmObj* %argslist53326$f470783)
ret void
}

define tailcc void @proc_clo$ae48026(%struct.ScmObj* %env$ae48026,%struct.ScmObj* %current_45args53322) {
%stackaddr$env-ref54825 = alloca %struct.ScmObj*, align 8
%f47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48026, i64 0)
store %struct.ScmObj* %f47078, %struct.ScmObj** %stackaddr$env-ref54825
%stackaddr$env-ref54826 = alloca %struct.ScmObj*, align 8
%lst47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48026, i64 1)
store %struct.ScmObj* %lst47076, %struct.ScmObj** %stackaddr$env-ref54826
%stackaddr$env-ref54827 = alloca %struct.ScmObj*, align 8
%_37foldl147075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48026, i64 2)
store %struct.ScmObj* %_37foldl147075, %struct.ScmObj** %stackaddr$env-ref54827
%stackaddr$env-ref54828 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48026, i64 3)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref54828
%stackaddr$prim54829 = alloca %struct.ScmObj*, align 8
%_95k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53322)
store volatile %struct.ScmObj* %_95k47442, %struct.ScmObj** %stackaddr$prim54829, align 8
%stackaddr$prim54830 = alloca %struct.ScmObj*, align 8
%current_45args53323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53322)
store volatile %struct.ScmObj* %current_45args53323, %struct.ScmObj** %stackaddr$prim54830, align 8
%stackaddr$prim54831 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53323)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim54831, align 8
%stackaddr$prim54832 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47076)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim54832, align 8
%argslist53325$_37foldl1470750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54833 = alloca %struct.ScmObj*, align 8
%argslist53325$_37foldl1470751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47221, %struct.ScmObj* %argslist53325$_37foldl1470750)
store volatile %struct.ScmObj* %argslist53325$_37foldl1470751, %struct.ScmObj** %stackaddr$prim54833, align 8
%stackaddr$prim54834 = alloca %struct.ScmObj*, align 8
%argslist53325$_37foldl1470752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47220, %struct.ScmObj* %argslist53325$_37foldl1470751)
store volatile %struct.ScmObj* %argslist53325$_37foldl1470752, %struct.ScmObj** %stackaddr$prim54834, align 8
%stackaddr$prim54835 = alloca %struct.ScmObj*, align 8
%argslist53325$_37foldl1470753 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47078, %struct.ScmObj* %argslist53325$_37foldl1470752)
store volatile %struct.ScmObj* %argslist53325$_37foldl1470753, %struct.ScmObj** %stackaddr$prim54835, align 8
%stackaddr$prim54836 = alloca %struct.ScmObj*, align 8
%argslist53325$_37foldl1470754 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47441, %struct.ScmObj* %argslist53325$_37foldl1470753)
store volatile %struct.ScmObj* %argslist53325$_37foldl1470754, %struct.ScmObj** %stackaddr$prim54836, align 8
%clofunc54837 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147075)
musttail call tailcc void %clofunc54837(%struct.ScmObj* %_37foldl147075, %struct.ScmObj* %argslist53325$_37foldl1470754)
ret void
}

define tailcc void @proc_clo$ae47929(%struct.ScmObj* %env$ae47929,%struct.ScmObj* %current_45args53330) {
%stackaddr$prim54838 = alloca %struct.ScmObj*, align 8
%k47443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53330)
store volatile %struct.ScmObj* %k47443, %struct.ScmObj** %stackaddr$prim54838, align 8
%stackaddr$prim54839 = alloca %struct.ScmObj*, align 8
%current_45args53331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53330)
store volatile %struct.ScmObj* %current_45args53331, %struct.ScmObj** %stackaddr$prim54839, align 8
%stackaddr$prim54840 = alloca %struct.ScmObj*, align 8
%_37length47080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53331)
store volatile %struct.ScmObj* %_37length47080, %struct.ScmObj** %stackaddr$prim54840, align 8
%ae47931 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54841 = alloca %struct.ScmObj*, align 8
%fptrToInt54842 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47932 to i64
%ae47932 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54842)
store volatile %struct.ScmObj* %ae47932, %struct.ScmObj** %stackaddr$makeclosure54841, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47932, %struct.ScmObj* %_37length47080, i64 0)
%argslist53342$k474430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54843 = alloca %struct.ScmObj*, align 8
%argslist53342$k474431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47932, %struct.ScmObj* %argslist53342$k474430)
store volatile %struct.ScmObj* %argslist53342$k474431, %struct.ScmObj** %stackaddr$prim54843, align 8
%stackaddr$prim54844 = alloca %struct.ScmObj*, align 8
%argslist53342$k474432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47931, %struct.ScmObj* %argslist53342$k474431)
store volatile %struct.ScmObj* %argslist53342$k474432, %struct.ScmObj** %stackaddr$prim54844, align 8
%clofunc54845 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47443)
musttail call tailcc void %clofunc54845(%struct.ScmObj* %k47443, %struct.ScmObj* %argslist53342$k474432)
ret void
}

define tailcc void @proc_clo$ae47932(%struct.ScmObj* %env$ae47932,%struct.ScmObj* %current_45args53333) {
%stackaddr$env-ref54846 = alloca %struct.ScmObj*, align 8
%_37length47080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47932, i64 0)
store %struct.ScmObj* %_37length47080, %struct.ScmObj** %stackaddr$env-ref54846
%stackaddr$prim54847 = alloca %struct.ScmObj*, align 8
%k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53333)
store volatile %struct.ScmObj* %k47444, %struct.ScmObj** %stackaddr$prim54847, align 8
%stackaddr$prim54848 = alloca %struct.ScmObj*, align 8
%current_45args53334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53333)
store volatile %struct.ScmObj* %current_45args53334, %struct.ScmObj** %stackaddr$prim54848, align 8
%stackaddr$prim54849 = alloca %struct.ScmObj*, align 8
%lst47081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53334)
store volatile %struct.ScmObj* %lst47081, %struct.ScmObj** %stackaddr$prim54849, align 8
%stackaddr$prim54850 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47081)
store volatile %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$prim54850, align 8
%truthy$cmp54851 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47214)
%cmp$cmp54851 = icmp eq i64 %truthy$cmp54851, 1
br i1 %cmp$cmp54851, label %truebranch$cmp54851, label %falsebranch$cmp54851
truebranch$cmp54851:
%ae47936 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47937 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53336$k474440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54852 = alloca %struct.ScmObj*, align 8
%argslist53336$k474441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47937, %struct.ScmObj* %argslist53336$k474440)
store volatile %struct.ScmObj* %argslist53336$k474441, %struct.ScmObj** %stackaddr$prim54852, align 8
%stackaddr$prim54853 = alloca %struct.ScmObj*, align 8
%argslist53336$k474442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47936, %struct.ScmObj* %argslist53336$k474441)
store volatile %struct.ScmObj* %argslist53336$k474442, %struct.ScmObj** %stackaddr$prim54853, align 8
%clofunc54854 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47444)
musttail call tailcc void %clofunc54854(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist53336$k474442)
ret void
falsebranch$cmp54851:
%stackaddr$prim54855 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47081)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim54855, align 8
%stackaddr$makeclosure54856 = alloca %struct.ScmObj*, align 8
%fptrToInt54857 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47946 to i64
%ae47946 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54857)
store volatile %struct.ScmObj* %ae47946, %struct.ScmObj** %stackaddr$makeclosure54856, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47946, %struct.ScmObj* %k47444, i64 0)
%argslist53341$_37length470800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54858 = alloca %struct.ScmObj*, align 8
%argslist53341$_37length470801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47215, %struct.ScmObj* %argslist53341$_37length470800)
store volatile %struct.ScmObj* %argslist53341$_37length470801, %struct.ScmObj** %stackaddr$prim54858, align 8
%stackaddr$prim54859 = alloca %struct.ScmObj*, align 8
%argslist53341$_37length470802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47946, %struct.ScmObj* %argslist53341$_37length470801)
store volatile %struct.ScmObj* %argslist53341$_37length470802, %struct.ScmObj** %stackaddr$prim54859, align 8
%clofunc54860 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47080)
musttail call tailcc void %clofunc54860(%struct.ScmObj* %_37length47080, %struct.ScmObj* %argslist53341$_37length470802)
ret void
}

define tailcc void @proc_clo$ae47946(%struct.ScmObj* %env$ae47946,%struct.ScmObj* %current_45args53337) {
%stackaddr$env-ref54861 = alloca %struct.ScmObj*, align 8
%k47444 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47946, i64 0)
store %struct.ScmObj* %k47444, %struct.ScmObj** %stackaddr$env-ref54861
%stackaddr$prim54862 = alloca %struct.ScmObj*, align 8
%_95k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53337)
store volatile %struct.ScmObj* %_95k47445, %struct.ScmObj** %stackaddr$prim54862, align 8
%stackaddr$prim54863 = alloca %struct.ScmObj*, align 8
%current_45args53338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53337)
store volatile %struct.ScmObj* %current_45args53338, %struct.ScmObj** %stackaddr$prim54863, align 8
%stackaddr$prim54864 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53338)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim54864, align 8
%ae47948 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54865 = alloca %struct.ScmObj*, align 8
%cpsprim47446 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae47948, %struct.ScmObj* %anf_45bind47216)
store volatile %struct.ScmObj* %cpsprim47446, %struct.ScmObj** %stackaddr$prim54865, align 8
%ae47951 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53340$k474440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54866 = alloca %struct.ScmObj*, align 8
%argslist53340$k474441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47446, %struct.ScmObj* %argslist53340$k474440)
store volatile %struct.ScmObj* %argslist53340$k474441, %struct.ScmObj** %stackaddr$prim54866, align 8
%stackaddr$prim54867 = alloca %struct.ScmObj*, align 8
%argslist53340$k474442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47951, %struct.ScmObj* %argslist53340$k474441)
store volatile %struct.ScmObj* %argslist53340$k474442, %struct.ScmObj** %stackaddr$prim54867, align 8
%clofunc54868 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47444)
musttail call tailcc void %clofunc54868(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist53340$k474442)
ret void
}

define tailcc void @proc_clo$ae47779(%struct.ScmObj* %env$ae47779,%struct.ScmObj* %current_45args53345) {
%stackaddr$prim54869 = alloca %struct.ScmObj*, align 8
%k47447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53345)
store volatile %struct.ScmObj* %k47447, %struct.ScmObj** %stackaddr$prim54869, align 8
%stackaddr$prim54870 = alloca %struct.ScmObj*, align 8
%current_45args53346 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53345)
store volatile %struct.ScmObj* %current_45args53346, %struct.ScmObj** %stackaddr$prim54870, align 8
%stackaddr$prim54871 = alloca %struct.ScmObj*, align 8
%_37take47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53346)
store volatile %struct.ScmObj* %_37take47083, %struct.ScmObj** %stackaddr$prim54871, align 8
%ae47781 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54872 = alloca %struct.ScmObj*, align 8
%fptrToInt54873 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47782 to i64
%ae47782 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54873)
store volatile %struct.ScmObj* %ae47782, %struct.ScmObj** %stackaddr$makeclosure54872, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47782, %struct.ScmObj* %_37take47083, i64 0)
%argslist53359$k474470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54874 = alloca %struct.ScmObj*, align 8
%argslist53359$k474471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47782, %struct.ScmObj* %argslist53359$k474470)
store volatile %struct.ScmObj* %argslist53359$k474471, %struct.ScmObj** %stackaddr$prim54874, align 8
%stackaddr$prim54875 = alloca %struct.ScmObj*, align 8
%argslist53359$k474472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47781, %struct.ScmObj* %argslist53359$k474471)
store volatile %struct.ScmObj* %argslist53359$k474472, %struct.ScmObj** %stackaddr$prim54875, align 8
%clofunc54876 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47447)
musttail call tailcc void %clofunc54876(%struct.ScmObj* %k47447, %struct.ScmObj* %argslist53359$k474472)
ret void
}

define tailcc void @proc_clo$ae47782(%struct.ScmObj* %env$ae47782,%struct.ScmObj* %current_45args53348) {
%stackaddr$env-ref54877 = alloca %struct.ScmObj*, align 8
%_37take47083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47782, i64 0)
store %struct.ScmObj* %_37take47083, %struct.ScmObj** %stackaddr$env-ref54877
%stackaddr$prim54878 = alloca %struct.ScmObj*, align 8
%k47448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53348)
store volatile %struct.ScmObj* %k47448, %struct.ScmObj** %stackaddr$prim54878, align 8
%stackaddr$prim54879 = alloca %struct.ScmObj*, align 8
%current_45args53349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53348)
store volatile %struct.ScmObj* %current_45args53349, %struct.ScmObj** %stackaddr$prim54879, align 8
%stackaddr$prim54880 = alloca %struct.ScmObj*, align 8
%lst47085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53349)
store volatile %struct.ScmObj* %lst47085, %struct.ScmObj** %stackaddr$prim54880, align 8
%stackaddr$prim54881 = alloca %struct.ScmObj*, align 8
%current_45args53350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53349)
store volatile %struct.ScmObj* %current_45args53350, %struct.ScmObj** %stackaddr$prim54881, align 8
%stackaddr$prim54882 = alloca %struct.ScmObj*, align 8
%n47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53350)
store volatile %struct.ScmObj* %n47084, %struct.ScmObj** %stackaddr$prim54882, align 8
%ae47784 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54883 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47084, %struct.ScmObj* %ae47784)
store volatile %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$prim54883, align 8
%truthy$cmp54884 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47207)
%cmp$cmp54884 = icmp eq i64 %truthy$cmp54884, 1
br i1 %cmp$cmp54884, label %truebranch$cmp54884, label %falsebranch$cmp54884
truebranch$cmp54884:
%ae47787 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47788 = call %struct.ScmObj* @const_init_null()
%argslist53352$k474480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54885 = alloca %struct.ScmObj*, align 8
%argslist53352$k474481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47788, %struct.ScmObj* %argslist53352$k474480)
store volatile %struct.ScmObj* %argslist53352$k474481, %struct.ScmObj** %stackaddr$prim54885, align 8
%stackaddr$prim54886 = alloca %struct.ScmObj*, align 8
%argslist53352$k474482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47787, %struct.ScmObj* %argslist53352$k474481)
store volatile %struct.ScmObj* %argslist53352$k474482, %struct.ScmObj** %stackaddr$prim54886, align 8
%clofunc54887 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47448)
musttail call tailcc void %clofunc54887(%struct.ScmObj* %k47448, %struct.ScmObj* %argslist53352$k474482)
ret void
falsebranch$cmp54884:
%stackaddr$prim54888 = alloca %struct.ScmObj*, align 8
%anf_45bind47208 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47085)
store volatile %struct.ScmObj* %anf_45bind47208, %struct.ScmObj** %stackaddr$prim54888, align 8
%truthy$cmp54889 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47208)
%cmp$cmp54889 = icmp eq i64 %truthy$cmp54889, 1
br i1 %cmp$cmp54889, label %truebranch$cmp54889, label %falsebranch$cmp54889
truebranch$cmp54889:
%ae47798 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47799 = call %struct.ScmObj* @const_init_null()
%argslist53353$k474480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54890 = alloca %struct.ScmObj*, align 8
%argslist53353$k474481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47799, %struct.ScmObj* %argslist53353$k474480)
store volatile %struct.ScmObj* %argslist53353$k474481, %struct.ScmObj** %stackaddr$prim54890, align 8
%stackaddr$prim54891 = alloca %struct.ScmObj*, align 8
%argslist53353$k474482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47798, %struct.ScmObj* %argslist53353$k474481)
store volatile %struct.ScmObj* %argslist53353$k474482, %struct.ScmObj** %stackaddr$prim54891, align 8
%clofunc54892 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47448)
musttail call tailcc void %clofunc54892(%struct.ScmObj* %k47448, %struct.ScmObj* %argslist53353$k474482)
ret void
falsebranch$cmp54889:
%stackaddr$prim54893 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47085)
store volatile %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$prim54893, align 8
%stackaddr$prim54894 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47085)
store volatile %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$prim54894, align 8
%ae47809 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54895 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47084, %struct.ScmObj* %ae47809)
store volatile %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$prim54895, align 8
%stackaddr$makeclosure54896 = alloca %struct.ScmObj*, align 8
%fptrToInt54897 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47811 to i64
%ae47811 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54897)
store volatile %struct.ScmObj* %ae47811, %struct.ScmObj** %stackaddr$makeclosure54896, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47811, %struct.ScmObj* %anf_45bind47209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47811, %struct.ScmObj* %k47448, i64 1)
%argslist53358$_37take470830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54898 = alloca %struct.ScmObj*, align 8
%argslist53358$_37take470831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47211, %struct.ScmObj* %argslist53358$_37take470830)
store volatile %struct.ScmObj* %argslist53358$_37take470831, %struct.ScmObj** %stackaddr$prim54898, align 8
%stackaddr$prim54899 = alloca %struct.ScmObj*, align 8
%argslist53358$_37take470832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47210, %struct.ScmObj* %argslist53358$_37take470831)
store volatile %struct.ScmObj* %argslist53358$_37take470832, %struct.ScmObj** %stackaddr$prim54899, align 8
%stackaddr$prim54900 = alloca %struct.ScmObj*, align 8
%argslist53358$_37take470833 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47811, %struct.ScmObj* %argslist53358$_37take470832)
store volatile %struct.ScmObj* %argslist53358$_37take470833, %struct.ScmObj** %stackaddr$prim54900, align 8
%clofunc54901 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47083)
musttail call tailcc void %clofunc54901(%struct.ScmObj* %_37take47083, %struct.ScmObj* %argslist53358$_37take470833)
ret void
}

define tailcc void @proc_clo$ae47811(%struct.ScmObj* %env$ae47811,%struct.ScmObj* %current_45args53354) {
%stackaddr$env-ref54902 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47811, i64 0)
store %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$env-ref54902
%stackaddr$env-ref54903 = alloca %struct.ScmObj*, align 8
%k47448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47811, i64 1)
store %struct.ScmObj* %k47448, %struct.ScmObj** %stackaddr$env-ref54903
%stackaddr$prim54904 = alloca %struct.ScmObj*, align 8
%_95k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53354)
store volatile %struct.ScmObj* %_95k47449, %struct.ScmObj** %stackaddr$prim54904, align 8
%stackaddr$prim54905 = alloca %struct.ScmObj*, align 8
%current_45args53355 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53354)
store volatile %struct.ScmObj* %current_45args53355, %struct.ScmObj** %stackaddr$prim54905, align 8
%stackaddr$prim54906 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53355)
store volatile %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$prim54906, align 8
%stackaddr$prim54907 = alloca %struct.ScmObj*, align 8
%cpsprim47450 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47209, %struct.ScmObj* %anf_45bind47212)
store volatile %struct.ScmObj* %cpsprim47450, %struct.ScmObj** %stackaddr$prim54907, align 8
%ae47817 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53357$k474480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54908 = alloca %struct.ScmObj*, align 8
%argslist53357$k474481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47450, %struct.ScmObj* %argslist53357$k474480)
store volatile %struct.ScmObj* %argslist53357$k474481, %struct.ScmObj** %stackaddr$prim54908, align 8
%stackaddr$prim54909 = alloca %struct.ScmObj*, align 8
%argslist53357$k474482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47817, %struct.ScmObj* %argslist53357$k474481)
store volatile %struct.ScmObj* %argslist53357$k474482, %struct.ScmObj** %stackaddr$prim54909, align 8
%clofunc54910 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47448)
musttail call tailcc void %clofunc54910(%struct.ScmObj* %k47448, %struct.ScmObj* %argslist53357$k474482)
ret void
}

define tailcc void @proc_clo$ae47682(%struct.ScmObj* %env$ae47682,%struct.ScmObj* %current_45args53362) {
%stackaddr$prim54911 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53362)
store volatile %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$prim54911, align 8
%stackaddr$prim54912 = alloca %struct.ScmObj*, align 8
%current_45args53363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53362)
store volatile %struct.ScmObj* %current_45args53363, %struct.ScmObj** %stackaddr$prim54912, align 8
%stackaddr$prim54913 = alloca %struct.ScmObj*, align 8
%_37map47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53363)
store volatile %struct.ScmObj* %_37map47087, %struct.ScmObj** %stackaddr$prim54913, align 8
%ae47684 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54914 = alloca %struct.ScmObj*, align 8
%fptrToInt54915 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47685 to i64
%ae47685 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54915)
store volatile %struct.ScmObj* %ae47685, %struct.ScmObj** %stackaddr$makeclosure54914, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47685, %struct.ScmObj* %_37map47087, i64 0)
%argslist53379$k474510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54916 = alloca %struct.ScmObj*, align 8
%argslist53379$k474511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47685, %struct.ScmObj* %argslist53379$k474510)
store volatile %struct.ScmObj* %argslist53379$k474511, %struct.ScmObj** %stackaddr$prim54916, align 8
%stackaddr$prim54917 = alloca %struct.ScmObj*, align 8
%argslist53379$k474512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47684, %struct.ScmObj* %argslist53379$k474511)
store volatile %struct.ScmObj* %argslist53379$k474512, %struct.ScmObj** %stackaddr$prim54917, align 8
%clofunc54918 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47451)
musttail call tailcc void %clofunc54918(%struct.ScmObj* %k47451, %struct.ScmObj* %argslist53379$k474512)
ret void
}

define tailcc void @proc_clo$ae47685(%struct.ScmObj* %env$ae47685,%struct.ScmObj* %current_45args53365) {
%stackaddr$env-ref54919 = alloca %struct.ScmObj*, align 8
%_37map47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47685, i64 0)
store %struct.ScmObj* %_37map47087, %struct.ScmObj** %stackaddr$env-ref54919
%stackaddr$prim54920 = alloca %struct.ScmObj*, align 8
%k47452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53365)
store volatile %struct.ScmObj* %k47452, %struct.ScmObj** %stackaddr$prim54920, align 8
%stackaddr$prim54921 = alloca %struct.ScmObj*, align 8
%current_45args53366 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53365)
store volatile %struct.ScmObj* %current_45args53366, %struct.ScmObj** %stackaddr$prim54921, align 8
%stackaddr$prim54922 = alloca %struct.ScmObj*, align 8
%f47089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53366)
store volatile %struct.ScmObj* %f47089, %struct.ScmObj** %stackaddr$prim54922, align 8
%stackaddr$prim54923 = alloca %struct.ScmObj*, align 8
%current_45args53367 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53366)
store volatile %struct.ScmObj* %current_45args53367, %struct.ScmObj** %stackaddr$prim54923, align 8
%stackaddr$prim54924 = alloca %struct.ScmObj*, align 8
%lst47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53367)
store volatile %struct.ScmObj* %lst47088, %struct.ScmObj** %stackaddr$prim54924, align 8
%stackaddr$prim54925 = alloca %struct.ScmObj*, align 8
%anf_45bind47201 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47201, %struct.ScmObj** %stackaddr$prim54925, align 8
%truthy$cmp54926 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47201)
%cmp$cmp54926 = icmp eq i64 %truthy$cmp54926, 1
br i1 %cmp$cmp54926, label %truebranch$cmp54926, label %falsebranch$cmp54926
truebranch$cmp54926:
%ae47689 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47690 = call %struct.ScmObj* @const_init_null()
%argslist53369$k474520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54927 = alloca %struct.ScmObj*, align 8
%argslist53369$k474521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47690, %struct.ScmObj* %argslist53369$k474520)
store volatile %struct.ScmObj* %argslist53369$k474521, %struct.ScmObj** %stackaddr$prim54927, align 8
%stackaddr$prim54928 = alloca %struct.ScmObj*, align 8
%argslist53369$k474522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47689, %struct.ScmObj* %argslist53369$k474521)
store volatile %struct.ScmObj* %argslist53369$k474522, %struct.ScmObj** %stackaddr$prim54928, align 8
%clofunc54929 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47452)
musttail call tailcc void %clofunc54929(%struct.ScmObj* %k47452, %struct.ScmObj* %argslist53369$k474522)
ret void
falsebranch$cmp54926:
%stackaddr$prim54930 = alloca %struct.ScmObj*, align 8
%anf_45bind47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47202, %struct.ScmObj** %stackaddr$prim54930, align 8
%stackaddr$makeclosure54931 = alloca %struct.ScmObj*, align 8
%fptrToInt54932 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47699 to i64
%ae47699 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54932)
store volatile %struct.ScmObj* %ae47699, %struct.ScmObj** %stackaddr$makeclosure54931, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47699, %struct.ScmObj* %f47089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47699, %struct.ScmObj* %lst47088, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47699, %struct.ScmObj* %_37map47087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47699, %struct.ScmObj* %k47452, i64 3)
%argslist53378$f470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54933 = alloca %struct.ScmObj*, align 8
%argslist53378$f470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47202, %struct.ScmObj* %argslist53378$f470890)
store volatile %struct.ScmObj* %argslist53378$f470891, %struct.ScmObj** %stackaddr$prim54933, align 8
%stackaddr$prim54934 = alloca %struct.ScmObj*, align 8
%argslist53378$f470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47699, %struct.ScmObj* %argslist53378$f470891)
store volatile %struct.ScmObj* %argslist53378$f470892, %struct.ScmObj** %stackaddr$prim54934, align 8
%clofunc54935 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47089)
musttail call tailcc void %clofunc54935(%struct.ScmObj* %f47089, %struct.ScmObj* %argslist53378$f470892)
ret void
}

define tailcc void @proc_clo$ae47699(%struct.ScmObj* %env$ae47699,%struct.ScmObj* %current_45args53370) {
%stackaddr$env-ref54936 = alloca %struct.ScmObj*, align 8
%f47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47699, i64 0)
store %struct.ScmObj* %f47089, %struct.ScmObj** %stackaddr$env-ref54936
%stackaddr$env-ref54937 = alloca %struct.ScmObj*, align 8
%lst47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47699, i64 1)
store %struct.ScmObj* %lst47088, %struct.ScmObj** %stackaddr$env-ref54937
%stackaddr$env-ref54938 = alloca %struct.ScmObj*, align 8
%_37map47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47699, i64 2)
store %struct.ScmObj* %_37map47087, %struct.ScmObj** %stackaddr$env-ref54938
%stackaddr$env-ref54939 = alloca %struct.ScmObj*, align 8
%k47452 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47699, i64 3)
store %struct.ScmObj* %k47452, %struct.ScmObj** %stackaddr$env-ref54939
%stackaddr$prim54940 = alloca %struct.ScmObj*, align 8
%_95k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53370)
store volatile %struct.ScmObj* %_95k47453, %struct.ScmObj** %stackaddr$prim54940, align 8
%stackaddr$prim54941 = alloca %struct.ScmObj*, align 8
%current_45args53371 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53370)
store volatile %struct.ScmObj* %current_45args53371, %struct.ScmObj** %stackaddr$prim54941, align 8
%stackaddr$prim54942 = alloca %struct.ScmObj*, align 8
%anf_45bind47203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53371)
store volatile %struct.ScmObj* %anf_45bind47203, %struct.ScmObj** %stackaddr$prim54942, align 8
%stackaddr$prim54943 = alloca %struct.ScmObj*, align 8
%anf_45bind47204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47204, %struct.ScmObj** %stackaddr$prim54943, align 8
%stackaddr$makeclosure54944 = alloca %struct.ScmObj*, align 8
%fptrToInt54945 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47703 to i64
%ae47703 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54945)
store volatile %struct.ScmObj* %ae47703, %struct.ScmObj** %stackaddr$makeclosure54944, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47703, %struct.ScmObj* %anf_45bind47203, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47703, %struct.ScmObj* %k47452, i64 1)
%argslist53377$_37map470870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54946 = alloca %struct.ScmObj*, align 8
%argslist53377$_37map470871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47204, %struct.ScmObj* %argslist53377$_37map470870)
store volatile %struct.ScmObj* %argslist53377$_37map470871, %struct.ScmObj** %stackaddr$prim54946, align 8
%stackaddr$prim54947 = alloca %struct.ScmObj*, align 8
%argslist53377$_37map470872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47089, %struct.ScmObj* %argslist53377$_37map470871)
store volatile %struct.ScmObj* %argslist53377$_37map470872, %struct.ScmObj** %stackaddr$prim54947, align 8
%stackaddr$prim54948 = alloca %struct.ScmObj*, align 8
%argslist53377$_37map470873 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47703, %struct.ScmObj* %argslist53377$_37map470872)
store volatile %struct.ScmObj* %argslist53377$_37map470873, %struct.ScmObj** %stackaddr$prim54948, align 8
%clofunc54949 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47087)
musttail call tailcc void %clofunc54949(%struct.ScmObj* %_37map47087, %struct.ScmObj* %argslist53377$_37map470873)
ret void
}

define tailcc void @proc_clo$ae47703(%struct.ScmObj* %env$ae47703,%struct.ScmObj* %current_45args53373) {
%stackaddr$env-ref54950 = alloca %struct.ScmObj*, align 8
%anf_45bind47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47703, i64 0)
store %struct.ScmObj* %anf_45bind47203, %struct.ScmObj** %stackaddr$env-ref54950
%stackaddr$env-ref54951 = alloca %struct.ScmObj*, align 8
%k47452 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47703, i64 1)
store %struct.ScmObj* %k47452, %struct.ScmObj** %stackaddr$env-ref54951
%stackaddr$prim54952 = alloca %struct.ScmObj*, align 8
%_95k47454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53373)
store volatile %struct.ScmObj* %_95k47454, %struct.ScmObj** %stackaddr$prim54952, align 8
%stackaddr$prim54953 = alloca %struct.ScmObj*, align 8
%current_45args53374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53373)
store volatile %struct.ScmObj* %current_45args53374, %struct.ScmObj** %stackaddr$prim54953, align 8
%stackaddr$prim54954 = alloca %struct.ScmObj*, align 8
%anf_45bind47205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53374)
store volatile %struct.ScmObj* %anf_45bind47205, %struct.ScmObj** %stackaddr$prim54954, align 8
%stackaddr$prim54955 = alloca %struct.ScmObj*, align 8
%cpsprim47455 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47203, %struct.ScmObj* %anf_45bind47205)
store volatile %struct.ScmObj* %cpsprim47455, %struct.ScmObj** %stackaddr$prim54955, align 8
%ae47709 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53376$k474520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54956 = alloca %struct.ScmObj*, align 8
%argslist53376$k474521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47455, %struct.ScmObj* %argslist53376$k474520)
store volatile %struct.ScmObj* %argslist53376$k474521, %struct.ScmObj** %stackaddr$prim54956, align 8
%stackaddr$prim54957 = alloca %struct.ScmObj*, align 8
%argslist53376$k474522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47709, %struct.ScmObj* %argslist53376$k474521)
store volatile %struct.ScmObj* %argslist53376$k474522, %struct.ScmObj** %stackaddr$prim54957, align 8
%clofunc54958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47452)
musttail call tailcc void %clofunc54958(%struct.ScmObj* %k47452, %struct.ScmObj* %argslist53376$k474522)
ret void
}

define tailcc void @proc_clo$ae47602(%struct.ScmObj* %env$ae47602,%struct.ScmObj* %current_45args53382) {
%stackaddr$prim54959 = alloca %struct.ScmObj*, align 8
%k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53382)
store volatile %struct.ScmObj* %k47456, %struct.ScmObj** %stackaddr$prim54959, align 8
%stackaddr$prim54960 = alloca %struct.ScmObj*, align 8
%current_45args53383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53382)
store volatile %struct.ScmObj* %current_45args53383, %struct.ScmObj** %stackaddr$prim54960, align 8
%stackaddr$prim54961 = alloca %struct.ScmObj*, align 8
%_37foldr147091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53383)
store volatile %struct.ScmObj* %_37foldr147091, %struct.ScmObj** %stackaddr$prim54961, align 8
%ae47604 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54962 = alloca %struct.ScmObj*, align 8
%fptrToInt54963 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47605 to i64
%ae47605 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54963)
store volatile %struct.ScmObj* %ae47605, %struct.ScmObj** %stackaddr$makeclosure54962, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47605, %struct.ScmObj* %_37foldr147091, i64 0)
%argslist53396$k474560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54964 = alloca %struct.ScmObj*, align 8
%argslist53396$k474561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47605, %struct.ScmObj* %argslist53396$k474560)
store volatile %struct.ScmObj* %argslist53396$k474561, %struct.ScmObj** %stackaddr$prim54964, align 8
%stackaddr$prim54965 = alloca %struct.ScmObj*, align 8
%argslist53396$k474562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47604, %struct.ScmObj* %argslist53396$k474561)
store volatile %struct.ScmObj* %argslist53396$k474562, %struct.ScmObj** %stackaddr$prim54965, align 8
%clofunc54966 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47456)
musttail call tailcc void %clofunc54966(%struct.ScmObj* %k47456, %struct.ScmObj* %argslist53396$k474562)
ret void
}

define tailcc void @proc_clo$ae47605(%struct.ScmObj* %env$ae47605,%struct.ScmObj* %current_45args53385) {
%stackaddr$env-ref54967 = alloca %struct.ScmObj*, align 8
%_37foldr147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47605, i64 0)
store %struct.ScmObj* %_37foldr147091, %struct.ScmObj** %stackaddr$env-ref54967
%stackaddr$prim54968 = alloca %struct.ScmObj*, align 8
%k47457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53385)
store volatile %struct.ScmObj* %k47457, %struct.ScmObj** %stackaddr$prim54968, align 8
%stackaddr$prim54969 = alloca %struct.ScmObj*, align 8
%current_45args53386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53385)
store volatile %struct.ScmObj* %current_45args53386, %struct.ScmObj** %stackaddr$prim54969, align 8
%stackaddr$prim54970 = alloca %struct.ScmObj*, align 8
%f47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53386)
store volatile %struct.ScmObj* %f47094, %struct.ScmObj** %stackaddr$prim54970, align 8
%stackaddr$prim54971 = alloca %struct.ScmObj*, align 8
%current_45args53387 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53386)
store volatile %struct.ScmObj* %current_45args53387, %struct.ScmObj** %stackaddr$prim54971, align 8
%stackaddr$prim54972 = alloca %struct.ScmObj*, align 8
%acc47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53387)
store volatile %struct.ScmObj* %acc47093, %struct.ScmObj** %stackaddr$prim54972, align 8
%stackaddr$prim54973 = alloca %struct.ScmObj*, align 8
%current_45args53388 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53387)
store volatile %struct.ScmObj* %current_45args53388, %struct.ScmObj** %stackaddr$prim54973, align 8
%stackaddr$prim54974 = alloca %struct.ScmObj*, align 8
%lst47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53388)
store volatile %struct.ScmObj* %lst47092, %struct.ScmObj** %stackaddr$prim54974, align 8
%stackaddr$prim54975 = alloca %struct.ScmObj*, align 8
%anf_45bind47196 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47196, %struct.ScmObj** %stackaddr$prim54975, align 8
%truthy$cmp54976 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47196)
%cmp$cmp54976 = icmp eq i64 %truthy$cmp54976, 1
br i1 %cmp$cmp54976, label %truebranch$cmp54976, label %falsebranch$cmp54976
truebranch$cmp54976:
%ae47609 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53390$k474570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54977 = alloca %struct.ScmObj*, align 8
%argslist53390$k474571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47093, %struct.ScmObj* %argslist53390$k474570)
store volatile %struct.ScmObj* %argslist53390$k474571, %struct.ScmObj** %stackaddr$prim54977, align 8
%stackaddr$prim54978 = alloca %struct.ScmObj*, align 8
%argslist53390$k474572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47609, %struct.ScmObj* %argslist53390$k474571)
store volatile %struct.ScmObj* %argslist53390$k474572, %struct.ScmObj** %stackaddr$prim54978, align 8
%clofunc54979 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47457)
musttail call tailcc void %clofunc54979(%struct.ScmObj* %k47457, %struct.ScmObj* %argslist53390$k474572)
ret void
falsebranch$cmp54976:
%stackaddr$prim54980 = alloca %struct.ScmObj*, align 8
%anf_45bind47197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47197, %struct.ScmObj** %stackaddr$prim54980, align 8
%stackaddr$prim54981 = alloca %struct.ScmObj*, align 8
%anf_45bind47198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47198, %struct.ScmObj** %stackaddr$prim54981, align 8
%stackaddr$makeclosure54982 = alloca %struct.ScmObj*, align 8
%fptrToInt54983 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47617 to i64
%ae47617 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54983)
store volatile %struct.ScmObj* %ae47617, %struct.ScmObj** %stackaddr$makeclosure54982, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47617, %struct.ScmObj* %f47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47617, %struct.ScmObj* %k47457, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47617, %struct.ScmObj* %anf_45bind47197, i64 2)
%argslist53395$_37foldr1470910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54984 = alloca %struct.ScmObj*, align 8
%argslist53395$_37foldr1470911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47198, %struct.ScmObj* %argslist53395$_37foldr1470910)
store volatile %struct.ScmObj* %argslist53395$_37foldr1470911, %struct.ScmObj** %stackaddr$prim54984, align 8
%stackaddr$prim54985 = alloca %struct.ScmObj*, align 8
%argslist53395$_37foldr1470912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47093, %struct.ScmObj* %argslist53395$_37foldr1470911)
store volatile %struct.ScmObj* %argslist53395$_37foldr1470912, %struct.ScmObj** %stackaddr$prim54985, align 8
%stackaddr$prim54986 = alloca %struct.ScmObj*, align 8
%argslist53395$_37foldr1470913 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47094, %struct.ScmObj* %argslist53395$_37foldr1470912)
store volatile %struct.ScmObj* %argslist53395$_37foldr1470913, %struct.ScmObj** %stackaddr$prim54986, align 8
%stackaddr$prim54987 = alloca %struct.ScmObj*, align 8
%argslist53395$_37foldr1470914 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47617, %struct.ScmObj* %argslist53395$_37foldr1470913)
store volatile %struct.ScmObj* %argslist53395$_37foldr1470914, %struct.ScmObj** %stackaddr$prim54987, align 8
%clofunc54988 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147091)
musttail call tailcc void %clofunc54988(%struct.ScmObj* %_37foldr147091, %struct.ScmObj* %argslist53395$_37foldr1470914)
ret void
}

define tailcc void @proc_clo$ae47617(%struct.ScmObj* %env$ae47617,%struct.ScmObj* %current_45args53391) {
%stackaddr$env-ref54989 = alloca %struct.ScmObj*, align 8
%f47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47617, i64 0)
store %struct.ScmObj* %f47094, %struct.ScmObj** %stackaddr$env-ref54989
%stackaddr$env-ref54990 = alloca %struct.ScmObj*, align 8
%k47457 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47617, i64 1)
store %struct.ScmObj* %k47457, %struct.ScmObj** %stackaddr$env-ref54990
%stackaddr$env-ref54991 = alloca %struct.ScmObj*, align 8
%anf_45bind47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47617, i64 2)
store %struct.ScmObj* %anf_45bind47197, %struct.ScmObj** %stackaddr$env-ref54991
%stackaddr$prim54992 = alloca %struct.ScmObj*, align 8
%_95k47458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53391)
store volatile %struct.ScmObj* %_95k47458, %struct.ScmObj** %stackaddr$prim54992, align 8
%stackaddr$prim54993 = alloca %struct.ScmObj*, align 8
%current_45args53392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53391)
store volatile %struct.ScmObj* %current_45args53392, %struct.ScmObj** %stackaddr$prim54993, align 8
%stackaddr$prim54994 = alloca %struct.ScmObj*, align 8
%anf_45bind47199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53392)
store volatile %struct.ScmObj* %anf_45bind47199, %struct.ScmObj** %stackaddr$prim54994, align 8
%argslist53394$f470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54995 = alloca %struct.ScmObj*, align 8
%argslist53394$f470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47199, %struct.ScmObj* %argslist53394$f470940)
store volatile %struct.ScmObj* %argslist53394$f470941, %struct.ScmObj** %stackaddr$prim54995, align 8
%stackaddr$prim54996 = alloca %struct.ScmObj*, align 8
%argslist53394$f470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47197, %struct.ScmObj* %argslist53394$f470941)
store volatile %struct.ScmObj* %argslist53394$f470942, %struct.ScmObj** %stackaddr$prim54996, align 8
%stackaddr$prim54997 = alloca %struct.ScmObj*, align 8
%argslist53394$f470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47457, %struct.ScmObj* %argslist53394$f470942)
store volatile %struct.ScmObj* %argslist53394$f470943, %struct.ScmObj** %stackaddr$prim54997, align 8
%clofunc54998 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47094)
musttail call tailcc void %clofunc54998(%struct.ScmObj* %f47094, %struct.ScmObj* %argslist53394$f470943)
ret void
}

define tailcc void @proc_clo$ae47485(%struct.ScmObj* %env$ae47485,%struct.ScmObj* %current_45args53399) {
%stackaddr$prim54999 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53399)
store volatile %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$prim54999, align 8
%stackaddr$prim55000 = alloca %struct.ScmObj*, align 8
%current_45args53400 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53399)
store volatile %struct.ScmObj* %current_45args53400, %struct.ScmObj** %stackaddr$prim55000, align 8
%stackaddr$prim55001 = alloca %struct.ScmObj*, align 8
%y47071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53400)
store volatile %struct.ScmObj* %y47071, %struct.ScmObj** %stackaddr$prim55001, align 8
%ae47487 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55002 = alloca %struct.ScmObj*, align 8
%fptrToInt55003 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47488 to i64
%ae47488 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55003)
store volatile %struct.ScmObj* %ae47488, %struct.ScmObj** %stackaddr$makeclosure55002, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47488, %struct.ScmObj* %y47071, i64 0)
%argslist53418$k474590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55004 = alloca %struct.ScmObj*, align 8
%argslist53418$k474591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47488, %struct.ScmObj* %argslist53418$k474590)
store volatile %struct.ScmObj* %argslist53418$k474591, %struct.ScmObj** %stackaddr$prim55004, align 8
%stackaddr$prim55005 = alloca %struct.ScmObj*, align 8
%argslist53418$k474592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47487, %struct.ScmObj* %argslist53418$k474591)
store volatile %struct.ScmObj* %argslist53418$k474592, %struct.ScmObj** %stackaddr$prim55005, align 8
%clofunc55006 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47459)
musttail call tailcc void %clofunc55006(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist53418$k474592)
ret void
}

define tailcc void @proc_clo$ae47488(%struct.ScmObj* %env$ae47488,%struct.ScmObj* %current_45args53402) {
%stackaddr$env-ref55007 = alloca %struct.ScmObj*, align 8
%y47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47488, i64 0)
store %struct.ScmObj* %y47071, %struct.ScmObj** %stackaddr$env-ref55007
%stackaddr$prim55008 = alloca %struct.ScmObj*, align 8
%k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53402)
store volatile %struct.ScmObj* %k47460, %struct.ScmObj** %stackaddr$prim55008, align 8
%stackaddr$prim55009 = alloca %struct.ScmObj*, align 8
%current_45args53403 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53402)
store volatile %struct.ScmObj* %current_45args53403, %struct.ScmObj** %stackaddr$prim55009, align 8
%stackaddr$prim55010 = alloca %struct.ScmObj*, align 8
%f47072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53403)
store volatile %struct.ScmObj* %f47072, %struct.ScmObj** %stackaddr$prim55010, align 8
%stackaddr$makeclosure55011 = alloca %struct.ScmObj*, align 8
%fptrToInt55012 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47489 to i64
%ae47489 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55012)
store volatile %struct.ScmObj* %ae47489, %struct.ScmObj** %stackaddr$makeclosure55011, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47489, %struct.ScmObj* %k47460, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47489, %struct.ScmObj* %f47072, i64 1)
%ae47490 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55013 = alloca %struct.ScmObj*, align 8
%fptrToInt55014 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47491 to i64
%ae47491 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55014)
store volatile %struct.ScmObj* %ae47491, %struct.ScmObj** %stackaddr$makeclosure55013, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47491, %struct.ScmObj* %f47072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47491, %struct.ScmObj* %y47071, i64 1)
%argslist53417$ae474890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55015 = alloca %struct.ScmObj*, align 8
%argslist53417$ae474891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47491, %struct.ScmObj* %argslist53417$ae474890)
store volatile %struct.ScmObj* %argslist53417$ae474891, %struct.ScmObj** %stackaddr$prim55015, align 8
%stackaddr$prim55016 = alloca %struct.ScmObj*, align 8
%argslist53417$ae474892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47490, %struct.ScmObj* %argslist53417$ae474891)
store volatile %struct.ScmObj* %argslist53417$ae474892, %struct.ScmObj** %stackaddr$prim55016, align 8
%clofunc55017 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47489)
musttail call tailcc void %clofunc55017(%struct.ScmObj* %ae47489, %struct.ScmObj* %argslist53417$ae474892)
ret void
}

define tailcc void @proc_clo$ae47489(%struct.ScmObj* %env$ae47489,%struct.ScmObj* %current_45args53405) {
%stackaddr$env-ref55018 = alloca %struct.ScmObj*, align 8
%k47460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47489, i64 0)
store %struct.ScmObj* %k47460, %struct.ScmObj** %stackaddr$env-ref55018
%stackaddr$env-ref55019 = alloca %struct.ScmObj*, align 8
%f47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47489, i64 1)
store %struct.ScmObj* %f47072, %struct.ScmObj** %stackaddr$env-ref55019
%stackaddr$prim55020 = alloca %struct.ScmObj*, align 8
%_95k47461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53405)
store volatile %struct.ScmObj* %_95k47461, %struct.ScmObj** %stackaddr$prim55020, align 8
%stackaddr$prim55021 = alloca %struct.ScmObj*, align 8
%current_45args53406 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53405)
store volatile %struct.ScmObj* %current_45args53406, %struct.ScmObj** %stackaddr$prim55021, align 8
%stackaddr$prim55022 = alloca %struct.ScmObj*, align 8
%anf_45bind47194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53406)
store volatile %struct.ScmObj* %anf_45bind47194, %struct.ScmObj** %stackaddr$prim55022, align 8
%argslist53408$f470720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55023 = alloca %struct.ScmObj*, align 8
%argslist53408$f470721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47194, %struct.ScmObj* %argslist53408$f470720)
store volatile %struct.ScmObj* %argslist53408$f470721, %struct.ScmObj** %stackaddr$prim55023, align 8
%stackaddr$prim55024 = alloca %struct.ScmObj*, align 8
%argslist53408$f470722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47460, %struct.ScmObj* %argslist53408$f470721)
store volatile %struct.ScmObj* %argslist53408$f470722, %struct.ScmObj** %stackaddr$prim55024, align 8
%clofunc55025 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47072)
musttail call tailcc void %clofunc55025(%struct.ScmObj* %f47072, %struct.ScmObj* %argslist53408$f470722)
ret void
}

define tailcc void @proc_clo$ae47491(%struct.ScmObj* %env$ae47491,%struct.ScmObj* %args4707347462) {
%stackaddr$env-ref55026 = alloca %struct.ScmObj*, align 8
%f47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47491, i64 0)
store %struct.ScmObj* %f47072, %struct.ScmObj** %stackaddr$env-ref55026
%stackaddr$env-ref55027 = alloca %struct.ScmObj*, align 8
%y47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47491, i64 1)
store %struct.ScmObj* %y47071, %struct.ScmObj** %stackaddr$env-ref55027
%stackaddr$prim55028 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4707347462)
store volatile %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$prim55028, align 8
%stackaddr$prim55029 = alloca %struct.ScmObj*, align 8
%args47073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4707347462)
store volatile %struct.ScmObj* %args47073, %struct.ScmObj** %stackaddr$prim55029, align 8
%stackaddr$makeclosure55030 = alloca %struct.ScmObj*, align 8
%fptrToInt55031 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47495 to i64
%ae47495 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55031)
store volatile %struct.ScmObj* %ae47495, %struct.ScmObj** %stackaddr$makeclosure55030, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47495, %struct.ScmObj* %args47073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47495, %struct.ScmObj* %f47072, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47495, %struct.ScmObj* %k47463, i64 2)
%argslist53416$y470710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55032 = alloca %struct.ScmObj*, align 8
%argslist53416$y470711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47071, %struct.ScmObj* %argslist53416$y470710)
store volatile %struct.ScmObj* %argslist53416$y470711, %struct.ScmObj** %stackaddr$prim55032, align 8
%stackaddr$prim55033 = alloca %struct.ScmObj*, align 8
%argslist53416$y470712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47495, %struct.ScmObj* %argslist53416$y470711)
store volatile %struct.ScmObj* %argslist53416$y470712, %struct.ScmObj** %stackaddr$prim55033, align 8
%clofunc55034 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47071)
musttail call tailcc void %clofunc55034(%struct.ScmObj* %y47071, %struct.ScmObj* %argslist53416$y470712)
ret void
}

define tailcc void @proc_clo$ae47495(%struct.ScmObj* %env$ae47495,%struct.ScmObj* %current_45args53409) {
%stackaddr$env-ref55035 = alloca %struct.ScmObj*, align 8
%args47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47495, i64 0)
store %struct.ScmObj* %args47073, %struct.ScmObj** %stackaddr$env-ref55035
%stackaddr$env-ref55036 = alloca %struct.ScmObj*, align 8
%f47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47495, i64 1)
store %struct.ScmObj* %f47072, %struct.ScmObj** %stackaddr$env-ref55036
%stackaddr$env-ref55037 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47495, i64 2)
store %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$env-ref55037
%stackaddr$prim55038 = alloca %struct.ScmObj*, align 8
%_95k47464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53409)
store volatile %struct.ScmObj* %_95k47464, %struct.ScmObj** %stackaddr$prim55038, align 8
%stackaddr$prim55039 = alloca %struct.ScmObj*, align 8
%current_45args53410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53409)
store volatile %struct.ScmObj* %current_45args53410, %struct.ScmObj** %stackaddr$prim55039, align 8
%stackaddr$prim55040 = alloca %struct.ScmObj*, align 8
%anf_45bind47192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53410)
store volatile %struct.ScmObj* %anf_45bind47192, %struct.ScmObj** %stackaddr$prim55040, align 8
%stackaddr$makeclosure55041 = alloca %struct.ScmObj*, align 8
%fptrToInt55042 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47498 to i64
%ae47498 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55042)
store volatile %struct.ScmObj* %ae47498, %struct.ScmObj** %stackaddr$makeclosure55041, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47498, %struct.ScmObj* %args47073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47498, %struct.ScmObj* %k47463, i64 1)
%argslist53415$anf_45bind471920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55043 = alloca %struct.ScmObj*, align 8
%argslist53415$anf_45bind471921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47072, %struct.ScmObj* %argslist53415$anf_45bind471920)
store volatile %struct.ScmObj* %argslist53415$anf_45bind471921, %struct.ScmObj** %stackaddr$prim55043, align 8
%stackaddr$prim55044 = alloca %struct.ScmObj*, align 8
%argslist53415$anf_45bind471922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47498, %struct.ScmObj* %argslist53415$anf_45bind471921)
store volatile %struct.ScmObj* %argslist53415$anf_45bind471922, %struct.ScmObj** %stackaddr$prim55044, align 8
%clofunc55045 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47192)
musttail call tailcc void %clofunc55045(%struct.ScmObj* %anf_45bind47192, %struct.ScmObj* %argslist53415$anf_45bind471922)
ret void
}

define tailcc void @proc_clo$ae47498(%struct.ScmObj* %env$ae47498,%struct.ScmObj* %current_45args53412) {
%stackaddr$env-ref55046 = alloca %struct.ScmObj*, align 8
%args47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47498, i64 0)
store %struct.ScmObj* %args47073, %struct.ScmObj** %stackaddr$env-ref55046
%stackaddr$env-ref55047 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47498, i64 1)
store %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$env-ref55047
%stackaddr$prim55048 = alloca %struct.ScmObj*, align 8
%_95k47465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53412)
store volatile %struct.ScmObj* %_95k47465, %struct.ScmObj** %stackaddr$prim55048, align 8
%stackaddr$prim55049 = alloca %struct.ScmObj*, align 8
%current_45args53413 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53412)
store volatile %struct.ScmObj* %current_45args53413, %struct.ScmObj** %stackaddr$prim55049, align 8
%stackaddr$prim55050 = alloca %struct.ScmObj*, align 8
%anf_45bind47193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53413)
store volatile %struct.ScmObj* %anf_45bind47193, %struct.ScmObj** %stackaddr$prim55050, align 8
%stackaddr$prim55051 = alloca %struct.ScmObj*, align 8
%cpsargs47466 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47463, %struct.ScmObj* %args47073)
store volatile %struct.ScmObj* %cpsargs47466, %struct.ScmObj** %stackaddr$prim55051, align 8
%clofunc55052 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47193)
musttail call tailcc void %clofunc55052(%struct.ScmObj* %anf_45bind47193, %struct.ScmObj* %cpsargs47466)
ret void
}

define tailcc void @proc_clo$ae47470(%struct.ScmObj* %env$ae47470,%struct.ScmObj* %current_45args53420) {
%stackaddr$prim55053 = alloca %struct.ScmObj*, align 8
%k47467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53420)
store volatile %struct.ScmObj* %k47467, %struct.ScmObj** %stackaddr$prim55053, align 8
%stackaddr$prim55054 = alloca %struct.ScmObj*, align 8
%current_45args53421 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53420)
store volatile %struct.ScmObj* %current_45args53421, %struct.ScmObj** %stackaddr$prim55054, align 8
%stackaddr$prim55055 = alloca %struct.ScmObj*, align 8
%yu47070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53421)
store volatile %struct.ScmObj* %yu47070, %struct.ScmObj** %stackaddr$prim55055, align 8
%argslist53423$yu470700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55056 = alloca %struct.ScmObj*, align 8
%argslist53423$yu470701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47070, %struct.ScmObj* %argslist53423$yu470700)
store volatile %struct.ScmObj* %argslist53423$yu470701, %struct.ScmObj** %stackaddr$prim55056, align 8
%stackaddr$prim55057 = alloca %struct.ScmObj*, align 8
%argslist53423$yu470702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47467, %struct.ScmObj* %argslist53423$yu470701)
store volatile %struct.ScmObj* %argslist53423$yu470702, %struct.ScmObj** %stackaddr$prim55057, align 8
%clofunc55058 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47070)
musttail call tailcc void %clofunc55058(%struct.ScmObj* %yu47070, %struct.ScmObj* %argslist53423$yu470702)
ret void
}