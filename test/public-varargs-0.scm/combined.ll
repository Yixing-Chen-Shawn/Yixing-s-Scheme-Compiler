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

@global$sym$ae4389947119 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv46677 = call %struct.ScmObj* @const_init_null()
%mainargs46678 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv46677, %struct.ScmObj* %mainargs46678)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv46675,%struct.ScmObj* %mainargs46676) {
%stackaddr$makeclosure46679 = alloca %struct.ScmObj*, align 8
%fptrToInt46680 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40511 to i64
%ae40511 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46680)
store volatile %struct.ScmObj* %ae40511, %struct.ScmObj** %stackaddr$makeclosure46679, align 8
%ae40512 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46681 = alloca %struct.ScmObj*, align 8
%fptrToInt46682 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40513 to i64
%ae40513 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46682)
store volatile %struct.ScmObj* %ae40513, %struct.ScmObj** %stackaddr$makeclosure46681, align 8
%args46674$ae40511$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46683 = alloca %struct.ScmObj*, align 8
%args46674$ae40511$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40513, %struct.ScmObj* %args46674$ae40511$0)
store volatile %struct.ScmObj* %args46674$ae40511$1, %struct.ScmObj** %stackaddr$prim46683, align 8
%stackaddr$prim46684 = alloca %struct.ScmObj*, align 8
%args46674$ae40511$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40512, %struct.ScmObj* %args46674$ae40511$1)
store volatile %struct.ScmObj* %args46674$ae40511$2, %struct.ScmObj** %stackaddr$prim46684, align 8
%clofunc46685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40511)
musttail call tailcc void %clofunc46685(%struct.ScmObj* %ae40511, %struct.ScmObj* %args46674$ae40511$2)
ret void
}

define tailcc void @proc_clo$ae40511(%struct.ScmObj* %env$ae40511,%struct.ScmObj* %current_45args46129) {
%stackaddr$prim46686 = alloca %struct.ScmObj*, align 8
%_95k40343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46129)
store volatile %struct.ScmObj* %_95k40343, %struct.ScmObj** %stackaddr$prim46686, align 8
%stackaddr$prim46687 = alloca %struct.ScmObj*, align 8
%current_45args46130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46129)
store volatile %struct.ScmObj* %current_45args46130, %struct.ScmObj** %stackaddr$prim46687, align 8
%stackaddr$prim46688 = alloca %struct.ScmObj*, align 8
%anf_45bind40227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46130)
store volatile %struct.ScmObj* %anf_45bind40227, %struct.ScmObj** %stackaddr$prim46688, align 8
%stackaddr$makeclosure46689 = alloca %struct.ScmObj*, align 8
%fptrToInt46690 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40526 to i64
%ae40526 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46690)
store volatile %struct.ScmObj* %ae40526, %struct.ScmObj** %stackaddr$makeclosure46689, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40526, %struct.ScmObj* %anf_45bind40227, i64 0)
%ae40527 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46691 = alloca %struct.ScmObj*, align 8
%fptrToInt46692 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40528 to i64
%ae40528 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46692)
store volatile %struct.ScmObj* %ae40528, %struct.ScmObj** %stackaddr$makeclosure46691, align 8
%args46669$ae40526$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46693 = alloca %struct.ScmObj*, align 8
%args46669$ae40526$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40528, %struct.ScmObj* %args46669$ae40526$0)
store volatile %struct.ScmObj* %args46669$ae40526$1, %struct.ScmObj** %stackaddr$prim46693, align 8
%stackaddr$prim46694 = alloca %struct.ScmObj*, align 8
%args46669$ae40526$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40527, %struct.ScmObj* %args46669$ae40526$1)
store volatile %struct.ScmObj* %args46669$ae40526$2, %struct.ScmObj** %stackaddr$prim46694, align 8
%clofunc46695 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40526)
musttail call tailcc void %clofunc46695(%struct.ScmObj* %ae40526, %struct.ScmObj* %args46669$ae40526$2)
ret void
}

define tailcc void @proc_clo$ae40526(%struct.ScmObj* %env$ae40526,%struct.ScmObj* %current_45args46132) {
%stackaddr$env-ref46696 = alloca %struct.ScmObj*, align 8
%anf_45bind40227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40526, i64 0)
store %struct.ScmObj* %anf_45bind40227, %struct.ScmObj** %stackaddr$env-ref46696
%stackaddr$prim46697 = alloca %struct.ScmObj*, align 8
%_95k40344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46132)
store volatile %struct.ScmObj* %_95k40344, %struct.ScmObj** %stackaddr$prim46697, align 8
%stackaddr$prim46698 = alloca %struct.ScmObj*, align 8
%current_45args46133 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46132)
store volatile %struct.ScmObj* %current_45args46133, %struct.ScmObj** %stackaddr$prim46698, align 8
%stackaddr$prim46699 = alloca %struct.ScmObj*, align 8
%anf_45bind40231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46133)
store volatile %struct.ScmObj* %anf_45bind40231, %struct.ScmObj** %stackaddr$prim46699, align 8
%stackaddr$makeclosure46700 = alloca %struct.ScmObj*, align 8
%fptrToInt46701 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40641 to i64
%ae40641 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46701)
store volatile %struct.ScmObj* %ae40641, %struct.ScmObj** %stackaddr$makeclosure46700, align 8
%args46648$anf_45bind40227$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46702 = alloca %struct.ScmObj*, align 8
%args46648$anf_45bind40227$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40231, %struct.ScmObj* %args46648$anf_45bind40227$0)
store volatile %struct.ScmObj* %args46648$anf_45bind40227$1, %struct.ScmObj** %stackaddr$prim46702, align 8
%stackaddr$prim46703 = alloca %struct.ScmObj*, align 8
%args46648$anf_45bind40227$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40641, %struct.ScmObj* %args46648$anf_45bind40227$1)
store volatile %struct.ScmObj* %args46648$anf_45bind40227$2, %struct.ScmObj** %stackaddr$prim46703, align 8
%clofunc46704 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40227)
musttail call tailcc void %clofunc46704(%struct.ScmObj* %anf_45bind40227, %struct.ScmObj* %args46648$anf_45bind40227$2)
ret void
}

define tailcc void @proc_clo$ae40641(%struct.ScmObj* %env$ae40641,%struct.ScmObj* %current_45args46135) {
%stackaddr$prim46705 = alloca %struct.ScmObj*, align 8
%_95k40345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46135)
store volatile %struct.ScmObj* %_95k40345, %struct.ScmObj** %stackaddr$prim46705, align 8
%stackaddr$prim46706 = alloca %struct.ScmObj*, align 8
%current_45args46136 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46135)
store volatile %struct.ScmObj* %current_45args46136, %struct.ScmObj** %stackaddr$prim46706, align 8
%stackaddr$prim46707 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46136)
store volatile %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$prim46707, align 8
%stackaddr$makeclosure46708 = alloca %struct.ScmObj*, align 8
%fptrToInt46709 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40643 to i64
%ae40643 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46709)
store volatile %struct.ScmObj* %ae40643, %struct.ScmObj** %stackaddr$makeclosure46708, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40643, %struct.ScmObj* %Ycmb40103, i64 0)
%ae40644 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46710 = alloca %struct.ScmObj*, align 8
%fptrToInt46711 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40645 to i64
%ae40645 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46711)
store volatile %struct.ScmObj* %ae40645, %struct.ScmObj** %stackaddr$makeclosure46710, align 8
%args46647$ae40643$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46712 = alloca %struct.ScmObj*, align 8
%args46647$ae40643$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40645, %struct.ScmObj* %args46647$ae40643$0)
store volatile %struct.ScmObj* %args46647$ae40643$1, %struct.ScmObj** %stackaddr$prim46712, align 8
%stackaddr$prim46713 = alloca %struct.ScmObj*, align 8
%args46647$ae40643$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40644, %struct.ScmObj* %args46647$ae40643$1)
store volatile %struct.ScmObj* %args46647$ae40643$2, %struct.ScmObj** %stackaddr$prim46713, align 8
%clofunc46714 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40643)
musttail call tailcc void %clofunc46714(%struct.ScmObj* %ae40643, %struct.ScmObj* %args46647$ae40643$2)
ret void
}

define tailcc void @proc_clo$ae40643(%struct.ScmObj* %env$ae40643,%struct.ScmObj* %current_45args46138) {
%stackaddr$env-ref46715 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40643, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46715
%stackaddr$prim46716 = alloca %struct.ScmObj*, align 8
%_95k40346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46138)
store volatile %struct.ScmObj* %_95k40346, %struct.ScmObj** %stackaddr$prim46716, align 8
%stackaddr$prim46717 = alloca %struct.ScmObj*, align 8
%current_45args46139 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46138)
store volatile %struct.ScmObj* %current_45args46139, %struct.ScmObj** %stackaddr$prim46717, align 8
%stackaddr$prim46718 = alloca %struct.ScmObj*, align 8
%anf_45bind40236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46139)
store volatile %struct.ScmObj* %anf_45bind40236, %struct.ScmObj** %stackaddr$prim46718, align 8
%stackaddr$makeclosure46719 = alloca %struct.ScmObj*, align 8
%fptrToInt46720 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40721 to i64
%ae40721 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46720)
store volatile %struct.ScmObj* %ae40721, %struct.ScmObj** %stackaddr$makeclosure46719, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40721, %struct.ScmObj* %Ycmb40103, i64 0)
%args46631$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46721 = alloca %struct.ScmObj*, align 8
%args46631$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40236, %struct.ScmObj* %args46631$Ycmb40103$0)
store volatile %struct.ScmObj* %args46631$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim46721, align 8
%stackaddr$prim46722 = alloca %struct.ScmObj*, align 8
%args46631$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40721, %struct.ScmObj* %args46631$Ycmb40103$1)
store volatile %struct.ScmObj* %args46631$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim46722, align 8
%clofunc46723 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc46723(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46631$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae40721(%struct.ScmObj* %env$ae40721,%struct.ScmObj* %current_45args46141) {
%stackaddr$env-ref46724 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40721, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46724
%stackaddr$prim46725 = alloca %struct.ScmObj*, align 8
%_95k40347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46141)
store volatile %struct.ScmObj* %_95k40347, %struct.ScmObj** %stackaddr$prim46725, align 8
%stackaddr$prim46726 = alloca %struct.ScmObj*, align 8
%current_45args46142 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46141)
store volatile %struct.ScmObj* %current_45args46142, %struct.ScmObj** %stackaddr$prim46726, align 8
%stackaddr$prim46727 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46142)
store volatile %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$prim46727, align 8
%stackaddr$makeclosure46728 = alloca %struct.ScmObj*, align 8
%fptrToInt46729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40723 to i64
%ae40723 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt46729)
store volatile %struct.ScmObj* %ae40723, %struct.ScmObj** %stackaddr$makeclosure46728, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40723, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40723, %struct.ScmObj* %_37foldr140124, i64 1)
%ae40724 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46730 = alloca %struct.ScmObj*, align 8
%fptrToInt46731 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40725 to i64
%ae40725 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46731)
store volatile %struct.ScmObj* %ae40725, %struct.ScmObj** %stackaddr$makeclosure46730, align 8
%args46630$ae40723$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46732 = alloca %struct.ScmObj*, align 8
%args46630$ae40723$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40725, %struct.ScmObj* %args46630$ae40723$0)
store volatile %struct.ScmObj* %args46630$ae40723$1, %struct.ScmObj** %stackaddr$prim46732, align 8
%stackaddr$prim46733 = alloca %struct.ScmObj*, align 8
%args46630$ae40723$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40724, %struct.ScmObj* %args46630$ae40723$1)
store volatile %struct.ScmObj* %args46630$ae40723$2, %struct.ScmObj** %stackaddr$prim46733, align 8
%clofunc46734 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40723)
musttail call tailcc void %clofunc46734(%struct.ScmObj* %ae40723, %struct.ScmObj* %args46630$ae40723$2)
ret void
}

define tailcc void @proc_clo$ae40723(%struct.ScmObj* %env$ae40723,%struct.ScmObj* %current_45args46144) {
%stackaddr$env-ref46735 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40723, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46735
%stackaddr$env-ref46736 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40723, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46736
%stackaddr$prim46737 = alloca %struct.ScmObj*, align 8
%_95k40348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46144)
store volatile %struct.ScmObj* %_95k40348, %struct.ScmObj** %stackaddr$prim46737, align 8
%stackaddr$prim46738 = alloca %struct.ScmObj*, align 8
%current_45args46145 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46144)
store volatile %struct.ScmObj* %current_45args46145, %struct.ScmObj** %stackaddr$prim46738, align 8
%stackaddr$prim46739 = alloca %struct.ScmObj*, align 8
%anf_45bind40242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46145)
store volatile %struct.ScmObj* %anf_45bind40242, %struct.ScmObj** %stackaddr$prim46739, align 8
%stackaddr$makeclosure46740 = alloca %struct.ScmObj*, align 8
%fptrToInt46741 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40818 to i64
%ae40818 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt46741)
store volatile %struct.ScmObj* %ae40818, %struct.ScmObj** %stackaddr$makeclosure46740, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40818, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40818, %struct.ScmObj* %_37foldr140124, i64 1)
%args46611$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46742 = alloca %struct.ScmObj*, align 8
%args46611$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40242, %struct.ScmObj* %args46611$Ycmb40103$0)
store volatile %struct.ScmObj* %args46611$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim46742, align 8
%stackaddr$prim46743 = alloca %struct.ScmObj*, align 8
%args46611$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40818, %struct.ScmObj* %args46611$Ycmb40103$1)
store volatile %struct.ScmObj* %args46611$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim46743, align 8
%clofunc46744 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc46744(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46611$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae40818(%struct.ScmObj* %env$ae40818,%struct.ScmObj* %current_45args46147) {
%stackaddr$env-ref46745 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40818, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46745
%stackaddr$env-ref46746 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40818, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46746
%stackaddr$prim46747 = alloca %struct.ScmObj*, align 8
%_95k40349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46147)
store volatile %struct.ScmObj* %_95k40349, %struct.ScmObj** %stackaddr$prim46747, align 8
%stackaddr$prim46748 = alloca %struct.ScmObj*, align 8
%current_45args46148 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46147)
store volatile %struct.ScmObj* %current_45args46148, %struct.ScmObj** %stackaddr$prim46748, align 8
%stackaddr$prim46749 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46148)
store volatile %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$prim46749, align 8
%stackaddr$makeclosure46750 = alloca %struct.ScmObj*, align 8
%fptrToInt46751 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40820 to i64
%ae40820 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt46751)
store volatile %struct.ScmObj* %ae40820, %struct.ScmObj** %stackaddr$makeclosure46750, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40820, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40820, %struct.ScmObj* %Ycmb40103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40820, %struct.ScmObj* %_37foldr140124, i64 2)
%ae40821 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46752 = alloca %struct.ScmObj*, align 8
%fptrToInt46753 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40822 to i64
%ae40822 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46753)
store volatile %struct.ScmObj* %ae40822, %struct.ScmObj** %stackaddr$makeclosure46752, align 8
%args46610$ae40820$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46754 = alloca %struct.ScmObj*, align 8
%args46610$ae40820$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40822, %struct.ScmObj* %args46610$ae40820$0)
store volatile %struct.ScmObj* %args46610$ae40820$1, %struct.ScmObj** %stackaddr$prim46754, align 8
%stackaddr$prim46755 = alloca %struct.ScmObj*, align 8
%args46610$ae40820$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40821, %struct.ScmObj* %args46610$ae40820$1)
store volatile %struct.ScmObj* %args46610$ae40820$2, %struct.ScmObj** %stackaddr$prim46755, align 8
%clofunc46756 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40820)
musttail call tailcc void %clofunc46756(%struct.ScmObj* %ae40820, %struct.ScmObj* %args46610$ae40820$2)
ret void
}

define tailcc void @proc_clo$ae40820(%struct.ScmObj* %env$ae40820,%struct.ScmObj* %current_45args46150) {
%stackaddr$env-ref46757 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40820, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref46757
%stackaddr$env-ref46758 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40820, i64 1)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46758
%stackaddr$env-ref46759 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40820, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46759
%stackaddr$prim46760 = alloca %struct.ScmObj*, align 8
%_95k40350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46150)
store volatile %struct.ScmObj* %_95k40350, %struct.ScmObj** %stackaddr$prim46760, align 8
%stackaddr$prim46761 = alloca %struct.ScmObj*, align 8
%current_45args46151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46150)
store volatile %struct.ScmObj* %current_45args46151, %struct.ScmObj** %stackaddr$prim46761, align 8
%stackaddr$prim46762 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46151)
store volatile %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$prim46762, align 8
%stackaddr$makeclosure46763 = alloca %struct.ScmObj*, align 8
%fptrToInt46764 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40968 to i64
%ae40968 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt46764)
store volatile %struct.ScmObj* %ae40968, %struct.ScmObj** %stackaddr$makeclosure46763, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40968, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40968, %struct.ScmObj* %Ycmb40103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40968, %struct.ScmObj* %_37foldr140124, i64 2)
%args46594$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46765 = alloca %struct.ScmObj*, align 8
%args46594$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40249, %struct.ScmObj* %args46594$Ycmb40103$0)
store volatile %struct.ScmObj* %args46594$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim46765, align 8
%stackaddr$prim46766 = alloca %struct.ScmObj*, align 8
%args46594$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40968, %struct.ScmObj* %args46594$Ycmb40103$1)
store volatile %struct.ScmObj* %args46594$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim46766, align 8
%clofunc46767 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc46767(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46594$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae40968(%struct.ScmObj* %env$ae40968,%struct.ScmObj* %current_45args46153) {
%stackaddr$env-ref46768 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40968, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref46768
%stackaddr$env-ref46769 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40968, i64 1)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46769
%stackaddr$env-ref46770 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40968, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46770
%stackaddr$prim46771 = alloca %struct.ScmObj*, align 8
%_95k40351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46153)
store volatile %struct.ScmObj* %_95k40351, %struct.ScmObj** %stackaddr$prim46771, align 8
%stackaddr$prim46772 = alloca %struct.ScmObj*, align 8
%current_45args46154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46153)
store volatile %struct.ScmObj* %current_45args46154, %struct.ScmObj** %stackaddr$prim46772, align 8
%stackaddr$prim46773 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46154)
store volatile %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$prim46773, align 8
%stackaddr$makeclosure46774 = alloca %struct.ScmObj*, align 8
%fptrToInt46775 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40970 to i64
%ae40970 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt46775)
store volatile %struct.ScmObj* %ae40970, %struct.ScmObj** %stackaddr$makeclosure46774, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40970, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40970, %struct.ScmObj* %Ycmb40103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40970, %struct.ScmObj* %_37take40116, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40970, %struct.ScmObj* %_37foldr140124, i64 3)
%ae40971 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46776 = alloca %struct.ScmObj*, align 8
%fptrToInt46777 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40972 to i64
%ae40972 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46777)
store volatile %struct.ScmObj* %ae40972, %struct.ScmObj** %stackaddr$makeclosure46776, align 8
%args46593$ae40970$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46778 = alloca %struct.ScmObj*, align 8
%args46593$ae40970$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40972, %struct.ScmObj* %args46593$ae40970$0)
store volatile %struct.ScmObj* %args46593$ae40970$1, %struct.ScmObj** %stackaddr$prim46778, align 8
%stackaddr$prim46779 = alloca %struct.ScmObj*, align 8
%args46593$ae40970$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40971, %struct.ScmObj* %args46593$ae40970$1)
store volatile %struct.ScmObj* %args46593$ae40970$2, %struct.ScmObj** %stackaddr$prim46779, align 8
%clofunc46780 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40970)
musttail call tailcc void %clofunc46780(%struct.ScmObj* %ae40970, %struct.ScmObj* %args46593$ae40970$2)
ret void
}

define tailcc void @proc_clo$ae40970(%struct.ScmObj* %env$ae40970,%struct.ScmObj* %current_45args46156) {
%stackaddr$env-ref46781 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40970, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref46781
%stackaddr$env-ref46782 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40970, i64 1)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46782
%stackaddr$env-ref46783 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40970, i64 2)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref46783
%stackaddr$env-ref46784 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40970, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46784
%stackaddr$prim46785 = alloca %struct.ScmObj*, align 8
%_95k40352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46156)
store volatile %struct.ScmObj* %_95k40352, %struct.ScmObj** %stackaddr$prim46785, align 8
%stackaddr$prim46786 = alloca %struct.ScmObj*, align 8
%current_45args46157 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46156)
store volatile %struct.ScmObj* %current_45args46157, %struct.ScmObj** %stackaddr$prim46786, align 8
%stackaddr$prim46787 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46157)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim46787, align 8
%stackaddr$makeclosure46788 = alloca %struct.ScmObj*, align 8
%fptrToInt46789 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41051 to i64
%ae41051 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt46789)
store volatile %struct.ScmObj* %ae41051, %struct.ScmObj** %stackaddr$makeclosure46788, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41051, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41051, %struct.ScmObj* %Ycmb40103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41051, %struct.ScmObj* %_37take40116, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41051, %struct.ScmObj* %_37foldr140124, i64 3)
%args46579$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46790 = alloca %struct.ScmObj*, align 8
%args46579$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40253, %struct.ScmObj* %args46579$Ycmb40103$0)
store volatile %struct.ScmObj* %args46579$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim46790, align 8
%stackaddr$prim46791 = alloca %struct.ScmObj*, align 8
%args46579$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41051, %struct.ScmObj* %args46579$Ycmb40103$1)
store volatile %struct.ScmObj* %args46579$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim46791, align 8
%clofunc46792 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc46792(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46579$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae41051(%struct.ScmObj* %env$ae41051,%struct.ScmObj* %current_45args46159) {
%stackaddr$env-ref46793 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41051, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref46793
%stackaddr$env-ref46794 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41051, i64 1)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46794
%stackaddr$env-ref46795 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41051, i64 2)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref46795
%stackaddr$env-ref46796 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41051, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46796
%stackaddr$prim46797 = alloca %struct.ScmObj*, align 8
%_95k40353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46159)
store volatile %struct.ScmObj* %_95k40353, %struct.ScmObj** %stackaddr$prim46797, align 8
%stackaddr$prim46798 = alloca %struct.ScmObj*, align 8
%current_45args46160 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46159)
store volatile %struct.ScmObj* %current_45args46160, %struct.ScmObj** %stackaddr$prim46798, align 8
%stackaddr$prim46799 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46160)
store volatile %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$prim46799, align 8
%stackaddr$makeclosure46800 = alloca %struct.ScmObj*, align 8
%fptrToInt46801 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41053 to i64
%ae41053 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt46801)
store volatile %struct.ScmObj* %ae41053, %struct.ScmObj** %stackaddr$makeclosure46800, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41053, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41053, %struct.ScmObj* %Ycmb40103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41053, %struct.ScmObj* %_37take40116, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41053, %struct.ScmObj* %_37length40113, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41053, %struct.ScmObj* %_37foldr140124, i64 4)
%ae41054 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46802 = alloca %struct.ScmObj*, align 8
%fptrToInt46803 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41055 to i64
%ae41055 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46803)
store volatile %struct.ScmObj* %ae41055, %struct.ScmObj** %stackaddr$makeclosure46802, align 8
%args46578$ae41053$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46804 = alloca %struct.ScmObj*, align 8
%args46578$ae41053$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41055, %struct.ScmObj* %args46578$ae41053$0)
store volatile %struct.ScmObj* %args46578$ae41053$1, %struct.ScmObj** %stackaddr$prim46804, align 8
%stackaddr$prim46805 = alloca %struct.ScmObj*, align 8
%args46578$ae41053$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41054, %struct.ScmObj* %args46578$ae41053$1)
store volatile %struct.ScmObj* %args46578$ae41053$2, %struct.ScmObj** %stackaddr$prim46805, align 8
%clofunc46806 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41053)
musttail call tailcc void %clofunc46806(%struct.ScmObj* %ae41053, %struct.ScmObj* %args46578$ae41053$2)
ret void
}

define tailcc void @proc_clo$ae41053(%struct.ScmObj* %env$ae41053,%struct.ScmObj* %current_45args46162) {
%stackaddr$env-ref46807 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41053, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref46807
%stackaddr$env-ref46808 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41053, i64 1)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46808
%stackaddr$env-ref46809 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41053, i64 2)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref46809
%stackaddr$env-ref46810 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41053, i64 3)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref46810
%stackaddr$env-ref46811 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41053, i64 4)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46811
%stackaddr$prim46812 = alloca %struct.ScmObj*, align 8
%_95k40354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46162)
store volatile %struct.ScmObj* %_95k40354, %struct.ScmObj** %stackaddr$prim46812, align 8
%stackaddr$prim46813 = alloca %struct.ScmObj*, align 8
%current_45args46163 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46162)
store volatile %struct.ScmObj* %current_45args46163, %struct.ScmObj** %stackaddr$prim46813, align 8
%stackaddr$prim46814 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46163)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim46814, align 8
%stackaddr$makeclosure46815 = alloca %struct.ScmObj*, align 8
%fptrToInt46816 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41130 to i64
%ae41130 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt46816)
store volatile %struct.ScmObj* %ae41130, %struct.ScmObj** %stackaddr$makeclosure46815, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41130, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41130, %struct.ScmObj* %Ycmb40103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41130, %struct.ScmObj* %_37take40116, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41130, %struct.ScmObj* %_37length40113, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41130, %struct.ScmObj* %_37foldr140124, i64 4)
%args46562$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46817 = alloca %struct.ScmObj*, align 8
%args46562$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40258, %struct.ScmObj* %args46562$Ycmb40103$0)
store volatile %struct.ScmObj* %args46562$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim46817, align 8
%stackaddr$prim46818 = alloca %struct.ScmObj*, align 8
%args46562$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41130, %struct.ScmObj* %args46562$Ycmb40103$1)
store volatile %struct.ScmObj* %args46562$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim46818, align 8
%clofunc46819 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc46819(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46562$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae41130(%struct.ScmObj* %env$ae41130,%struct.ScmObj* %current_45args46165) {
%stackaddr$env-ref46820 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41130, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref46820
%stackaddr$env-ref46821 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41130, i64 1)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46821
%stackaddr$env-ref46822 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41130, i64 2)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref46822
%stackaddr$env-ref46823 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41130, i64 3)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref46823
%stackaddr$env-ref46824 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41130, i64 4)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46824
%stackaddr$prim46825 = alloca %struct.ScmObj*, align 8
%_95k40355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46165)
store volatile %struct.ScmObj* %_95k40355, %struct.ScmObj** %stackaddr$prim46825, align 8
%stackaddr$prim46826 = alloca %struct.ScmObj*, align 8
%current_45args46166 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46165)
store volatile %struct.ScmObj* %current_45args46166, %struct.ScmObj** %stackaddr$prim46826, align 8
%stackaddr$prim46827 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46166)
store volatile %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$prim46827, align 8
%stackaddr$makeclosure46828 = alloca %struct.ScmObj*, align 8
%fptrToInt46829 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41132 to i64
%ae41132 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt46829)
store volatile %struct.ScmObj* %ae41132, %struct.ScmObj** %stackaddr$makeclosure46828, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41132, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41132, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41132, %struct.ScmObj* %_37map140120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41132, %struct.ScmObj* %Ycmb40103, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41132, %struct.ScmObj* %_37take40116, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41132, %struct.ScmObj* %_37length40113, i64 5)
%ae41133 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46830 = alloca %struct.ScmObj*, align 8
%fptrToInt46831 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41134 to i64
%ae41134 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46831)
store volatile %struct.ScmObj* %ae41134, %struct.ScmObj** %stackaddr$makeclosure46830, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41134, %struct.ScmObj* %_37foldl140108, i64 0)
%args46561$ae41132$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46832 = alloca %struct.ScmObj*, align 8
%args46561$ae41132$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41134, %struct.ScmObj* %args46561$ae41132$0)
store volatile %struct.ScmObj* %args46561$ae41132$1, %struct.ScmObj** %stackaddr$prim46832, align 8
%stackaddr$prim46833 = alloca %struct.ScmObj*, align 8
%args46561$ae41132$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41133, %struct.ScmObj* %args46561$ae41132$1)
store volatile %struct.ScmObj* %args46561$ae41132$2, %struct.ScmObj** %stackaddr$prim46833, align 8
%clofunc46834 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41132)
musttail call tailcc void %clofunc46834(%struct.ScmObj* %ae41132, %struct.ScmObj* %args46561$ae41132$2)
ret void
}

define tailcc void @proc_clo$ae41132(%struct.ScmObj* %env$ae41132,%struct.ScmObj* %current_45args46168) {
%stackaddr$env-ref46835 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41132, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46835
%stackaddr$env-ref46836 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41132, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46836
%stackaddr$env-ref46837 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41132, i64 2)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref46837
%stackaddr$env-ref46838 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41132, i64 3)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46838
%stackaddr$env-ref46839 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41132, i64 4)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref46839
%stackaddr$env-ref46840 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41132, i64 5)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref46840
%stackaddr$prim46841 = alloca %struct.ScmObj*, align 8
%_95k40356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46168)
store volatile %struct.ScmObj* %_95k40356, %struct.ScmObj** %stackaddr$prim46841, align 8
%stackaddr$prim46842 = alloca %struct.ScmObj*, align 8
%current_45args46169 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46168)
store volatile %struct.ScmObj* %current_45args46169, %struct.ScmObj** %stackaddr$prim46842, align 8
%stackaddr$prim46843 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46169)
store volatile %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$prim46843, align 8
%stackaddr$makeclosure46844 = alloca %struct.ScmObj*, align 8
%fptrToInt46845 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41186 to i64
%ae41186 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt46845)
store volatile %struct.ScmObj* %ae41186, %struct.ScmObj** %stackaddr$makeclosure46844, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41186, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41186, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41186, %struct.ScmObj* %_37map140120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41186, %struct.ScmObj* %Ycmb40103, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41186, %struct.ScmObj* %_37last40146, i64 4)
%ae41187 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46846 = alloca %struct.ScmObj*, align 8
%fptrToInt46847 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41188 to i64
%ae41188 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt46847)
store volatile %struct.ScmObj* %ae41188, %struct.ScmObj** %stackaddr$makeclosure46846, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41188, %struct.ScmObj* %_37take40116, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41188, %struct.ScmObj* %_37length40113, i64 1)
%args46547$ae41186$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46848 = alloca %struct.ScmObj*, align 8
%args46547$ae41186$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41188, %struct.ScmObj* %args46547$ae41186$0)
store volatile %struct.ScmObj* %args46547$ae41186$1, %struct.ScmObj** %stackaddr$prim46848, align 8
%stackaddr$prim46849 = alloca %struct.ScmObj*, align 8
%args46547$ae41186$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41187, %struct.ScmObj* %args46547$ae41186$1)
store volatile %struct.ScmObj* %args46547$ae41186$2, %struct.ScmObj** %stackaddr$prim46849, align 8
%clofunc46850 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41186)
musttail call tailcc void %clofunc46850(%struct.ScmObj* %ae41186, %struct.ScmObj* %args46547$ae41186$2)
ret void
}

define tailcc void @proc_clo$ae41186(%struct.ScmObj* %env$ae41186,%struct.ScmObj* %current_45args46171) {
%stackaddr$env-ref46851 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41186, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46851
%stackaddr$env-ref46852 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41186, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46852
%stackaddr$env-ref46853 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41186, i64 2)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref46853
%stackaddr$env-ref46854 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41186, i64 3)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46854
%stackaddr$env-ref46855 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41186, i64 4)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref46855
%stackaddr$prim46856 = alloca %struct.ScmObj*, align 8
%_95k40357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46171)
store volatile %struct.ScmObj* %_95k40357, %struct.ScmObj** %stackaddr$prim46856, align 8
%stackaddr$prim46857 = alloca %struct.ScmObj*, align 8
%current_45args46172 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46171)
store volatile %struct.ScmObj* %current_45args46172, %struct.ScmObj** %stackaddr$prim46857, align 8
%stackaddr$prim46858 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46172)
store volatile %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$prim46858, align 8
%stackaddr$makeclosure46859 = alloca %struct.ScmObj*, align 8
%fptrToInt46860 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41216 to i64
%ae41216 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt46860)
store volatile %struct.ScmObj* %ae41216, %struct.ScmObj** %stackaddr$makeclosure46859, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41216, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41216, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41216, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41216, %struct.ScmObj* %_37last40146, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41216, %struct.ScmObj* %_37drop_45right40143, i64 4)
%ae41217 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46861 = alloca %struct.ScmObj*, align 8
%fptrToInt46862 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41218 to i64
%ae41218 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt46862)
store volatile %struct.ScmObj* %ae41218, %struct.ScmObj** %stackaddr$makeclosure46861, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41218, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41218, %struct.ScmObj* %_37foldr140124, i64 1)
%args46537$ae41216$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46863 = alloca %struct.ScmObj*, align 8
%args46537$ae41216$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41218, %struct.ScmObj* %args46537$ae41216$0)
store volatile %struct.ScmObj* %args46537$ae41216$1, %struct.ScmObj** %stackaddr$prim46863, align 8
%stackaddr$prim46864 = alloca %struct.ScmObj*, align 8
%args46537$ae41216$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41217, %struct.ScmObj* %args46537$ae41216$1)
store volatile %struct.ScmObj* %args46537$ae41216$2, %struct.ScmObj** %stackaddr$prim46864, align 8
%clofunc46865 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41216)
musttail call tailcc void %clofunc46865(%struct.ScmObj* %ae41216, %struct.ScmObj* %args46537$ae41216$2)
ret void
}

define tailcc void @proc_clo$ae41216(%struct.ScmObj* %env$ae41216,%struct.ScmObj* %current_45args46174) {
%stackaddr$env-ref46866 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41216, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46866
%stackaddr$env-ref46867 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41216, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46867
%stackaddr$env-ref46868 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41216, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46868
%stackaddr$env-ref46869 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41216, i64 3)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref46869
%stackaddr$env-ref46870 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41216, i64 4)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref46870
%stackaddr$prim46871 = alloca %struct.ScmObj*, align 8
%_95k40358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46174)
store volatile %struct.ScmObj* %_95k40358, %struct.ScmObj** %stackaddr$prim46871, align 8
%stackaddr$prim46872 = alloca %struct.ScmObj*, align 8
%current_45args46175 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46174)
store volatile %struct.ScmObj* %current_45args46175, %struct.ScmObj** %stackaddr$prim46872, align 8
%stackaddr$prim46873 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46175)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim46873, align 8
%stackaddr$makeclosure46874 = alloca %struct.ScmObj*, align 8
%fptrToInt46875 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41600 to i64
%ae41600 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt46875)
store volatile %struct.ScmObj* %ae41600, %struct.ScmObj** %stackaddr$makeclosure46874, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41600, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41600, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41600, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41600, %struct.ScmObj* %_37last40146, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41600, %struct.ScmObj* %_37drop_45right40143, i64 4)
%args46477$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46876 = alloca %struct.ScmObj*, align 8
%args46477$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40274, %struct.ScmObj* %args46477$Ycmb40103$0)
store volatile %struct.ScmObj* %args46477$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim46876, align 8
%stackaddr$prim46877 = alloca %struct.ScmObj*, align 8
%args46477$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41600, %struct.ScmObj* %args46477$Ycmb40103$1)
store volatile %struct.ScmObj* %args46477$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim46877, align 8
%clofunc46878 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc46878(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46477$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae41600(%struct.ScmObj* %env$ae41600,%struct.ScmObj* %current_45args46177) {
%stackaddr$env-ref46879 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41600, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46879
%stackaddr$env-ref46880 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41600, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46880
%stackaddr$env-ref46881 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41600, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46881
%stackaddr$env-ref46882 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41600, i64 3)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref46882
%stackaddr$env-ref46883 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41600, i64 4)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref46883
%stackaddr$prim46884 = alloca %struct.ScmObj*, align 8
%_95k40359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46177)
store volatile %struct.ScmObj* %_95k40359, %struct.ScmObj** %stackaddr$prim46884, align 8
%stackaddr$prim46885 = alloca %struct.ScmObj*, align 8
%current_45args46178 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46177)
store volatile %struct.ScmObj* %current_45args46178, %struct.ScmObj** %stackaddr$prim46885, align 8
%stackaddr$prim46886 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46178)
store volatile %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$prim46886, align 8
%stackaddr$makeclosure46887 = alloca %struct.ScmObj*, align 8
%fptrToInt46888 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41602 to i64
%ae41602 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt46888)
store volatile %struct.ScmObj* %ae41602, %struct.ScmObj** %stackaddr$makeclosure46887, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41602, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41602, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41602, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41602, %struct.ScmObj* %_37last40146, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41602, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41602, %struct.ScmObj* %_37drop_45right40143, i64 5)
%ae41603 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46889 = alloca %struct.ScmObj*, align 8
%fptrToInt46890 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41604 to i64
%ae41604 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46890)
store volatile %struct.ScmObj* %ae41604, %struct.ScmObj** %stackaddr$makeclosure46889, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41604, %struct.ScmObj* %_37foldr140124, i64 0)
%args46476$ae41602$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46891 = alloca %struct.ScmObj*, align 8
%args46476$ae41602$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41604, %struct.ScmObj* %args46476$ae41602$0)
store volatile %struct.ScmObj* %args46476$ae41602$1, %struct.ScmObj** %stackaddr$prim46891, align 8
%stackaddr$prim46892 = alloca %struct.ScmObj*, align 8
%args46476$ae41602$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41603, %struct.ScmObj* %args46476$ae41602$1)
store volatile %struct.ScmObj* %args46476$ae41602$2, %struct.ScmObj** %stackaddr$prim46892, align 8
%clofunc46893 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41602)
musttail call tailcc void %clofunc46893(%struct.ScmObj* %ae41602, %struct.ScmObj* %args46476$ae41602$2)
ret void
}

define tailcc void @proc_clo$ae41602(%struct.ScmObj* %env$ae41602,%struct.ScmObj* %current_45args46180) {
%stackaddr$env-ref46894 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41602, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46894
%stackaddr$env-ref46895 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41602, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46895
%stackaddr$env-ref46896 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41602, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46896
%stackaddr$env-ref46897 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41602, i64 3)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref46897
%stackaddr$env-ref46898 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41602, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref46898
%stackaddr$env-ref46899 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41602, i64 5)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref46899
%stackaddr$prim46900 = alloca %struct.ScmObj*, align 8
%_95k40360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46180)
store volatile %struct.ScmObj* %_95k40360, %struct.ScmObj** %stackaddr$prim46900, align 8
%stackaddr$prim46901 = alloca %struct.ScmObj*, align 8
%current_45args46181 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46180)
store volatile %struct.ScmObj* %current_45args46181, %struct.ScmObj** %stackaddr$prim46901, align 8
%stackaddr$prim46902 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46181)
store volatile %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$prim46902, align 8
%stackaddr$makeclosure46903 = alloca %struct.ScmObj*, align 8
%fptrToInt46904 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41679 to i64
%ae41679 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt46904)
store volatile %struct.ScmObj* %ae41679, %struct.ScmObj** %stackaddr$makeclosure46903, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41679, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41679, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41679, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41679, %struct.ScmObj* %_37foldr40129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41679, %struct.ScmObj* %_37map140155, i64 4)
%ae41680 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46905 = alloca %struct.ScmObj*, align 8
%fptrToInt46906 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41681 to i64
%ae41681 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt46906)
store volatile %struct.ScmObj* %ae41681, %struct.ScmObj** %stackaddr$makeclosure46905, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41681, %struct.ScmObj* %_37last40146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41681, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41681, %struct.ScmObj* %_37drop_45right40143, i64 2)
%args46457$ae41679$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46907 = alloca %struct.ScmObj*, align 8
%args46457$ae41679$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41681, %struct.ScmObj* %args46457$ae41679$0)
store volatile %struct.ScmObj* %args46457$ae41679$1, %struct.ScmObj** %stackaddr$prim46907, align 8
%stackaddr$prim46908 = alloca %struct.ScmObj*, align 8
%args46457$ae41679$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41680, %struct.ScmObj* %args46457$ae41679$1)
store volatile %struct.ScmObj* %args46457$ae41679$2, %struct.ScmObj** %stackaddr$prim46908, align 8
%clofunc46909 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41679)
musttail call tailcc void %clofunc46909(%struct.ScmObj* %ae41679, %struct.ScmObj* %args46457$ae41679$2)
ret void
}

define tailcc void @proc_clo$ae41679(%struct.ScmObj* %env$ae41679,%struct.ScmObj* %current_45args46183) {
%stackaddr$env-ref46910 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41679, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46910
%stackaddr$env-ref46911 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41679, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46911
%stackaddr$env-ref46912 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41679, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46912
%stackaddr$env-ref46913 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41679, i64 3)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref46913
%stackaddr$env-ref46914 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41679, i64 4)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref46914
%stackaddr$prim46915 = alloca %struct.ScmObj*, align 8
%_95k40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46183)
store volatile %struct.ScmObj* %_95k40361, %struct.ScmObj** %stackaddr$prim46915, align 8
%stackaddr$prim46916 = alloca %struct.ScmObj*, align 8
%current_45args46184 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46183)
store volatile %struct.ScmObj* %current_45args46184, %struct.ScmObj** %stackaddr$prim46916, align 8
%stackaddr$prim46917 = alloca %struct.ScmObj*, align 8
%_37map40150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46184)
store volatile %struct.ScmObj* %_37map40150, %struct.ScmObj** %stackaddr$prim46917, align 8
%stackaddr$makeclosure46918 = alloca %struct.ScmObj*, align 8
%fptrToInt46919 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41825 to i64
%ae41825 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt46919)
store volatile %struct.ScmObj* %ae41825, %struct.ScmObj** %stackaddr$makeclosure46918, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41825, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41825, %struct.ScmObj* %_37foldl140108, i64 1)
%ae41826 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46920 = alloca %struct.ScmObj*, align 8
%fptrToInt46921 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41827 to i64
%ae41827 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt46921)
store volatile %struct.ScmObj* %ae41827, %struct.ScmObj** %stackaddr$makeclosure46920, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41827, %struct.ScmObj* %_37foldr40129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41827, %struct.ScmObj* %_37foldr140124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41827, %struct.ScmObj* %_37map140155, i64 2)
%args46440$ae41825$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46922 = alloca %struct.ScmObj*, align 8
%args46440$ae41825$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41827, %struct.ScmObj* %args46440$ae41825$0)
store volatile %struct.ScmObj* %args46440$ae41825$1, %struct.ScmObj** %stackaddr$prim46922, align 8
%stackaddr$prim46923 = alloca %struct.ScmObj*, align 8
%args46440$ae41825$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41826, %struct.ScmObj* %args46440$ae41825$1)
store volatile %struct.ScmObj* %args46440$ae41825$2, %struct.ScmObj** %stackaddr$prim46923, align 8
%clofunc46924 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41825)
musttail call tailcc void %clofunc46924(%struct.ScmObj* %ae41825, %struct.ScmObj* %args46440$ae41825$2)
ret void
}

define tailcc void @proc_clo$ae41825(%struct.ScmObj* %env$ae41825,%struct.ScmObj* %current_45args46186) {
%stackaddr$env-ref46925 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41825, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46925
%stackaddr$env-ref46926 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41825, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46926
%stackaddr$prim46927 = alloca %struct.ScmObj*, align 8
%_95k40362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46186)
store volatile %struct.ScmObj* %_95k40362, %struct.ScmObj** %stackaddr$prim46927, align 8
%stackaddr$prim46928 = alloca %struct.ScmObj*, align 8
%current_45args46187 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46186)
store volatile %struct.ScmObj* %current_45args46187, %struct.ScmObj** %stackaddr$prim46928, align 8
%stackaddr$prim46929 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46187)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim46929, align 8
%stackaddr$makeclosure46930 = alloca %struct.ScmObj*, align 8
%fptrToInt46931 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42217 to i64
%ae42217 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46931)
store volatile %struct.ScmObj* %ae42217, %struct.ScmObj** %stackaddr$makeclosure46930, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42217, %struct.ScmObj* %_37foldl140108, i64 0)
%args46380$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46932 = alloca %struct.ScmObj*, align 8
%args46380$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40294, %struct.ScmObj* %args46380$Ycmb40103$0)
store volatile %struct.ScmObj* %args46380$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim46932, align 8
%stackaddr$prim46933 = alloca %struct.ScmObj*, align 8
%args46380$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42217, %struct.ScmObj* %args46380$Ycmb40103$1)
store volatile %struct.ScmObj* %args46380$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim46933, align 8
%clofunc46934 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc46934(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46380$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae42217(%struct.ScmObj* %env$ae42217,%struct.ScmObj* %current_45args46189) {
%stackaddr$env-ref46935 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42217, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46935
%stackaddr$prim46936 = alloca %struct.ScmObj*, align 8
%_95k40363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46189)
store volatile %struct.ScmObj* %_95k40363, %struct.ScmObj** %stackaddr$prim46936, align 8
%stackaddr$prim46937 = alloca %struct.ScmObj*, align 8
%current_45args46190 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46189)
store volatile %struct.ScmObj* %current_45args46190, %struct.ScmObj** %stackaddr$prim46937, align 8
%stackaddr$prim46938 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46190)
store volatile %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$prim46938, align 8
%stackaddr$makeclosure46939 = alloca %struct.ScmObj*, align 8
%fptrToInt46940 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42219 to i64
%ae42219 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46940)
store volatile %struct.ScmObj* %ae42219, %struct.ScmObj** %stackaddr$makeclosure46939, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42219, %struct.ScmObj* %_37foldl140108, i64 0)
%ae42220 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46941 = alloca %struct.ScmObj*, align 8
%fptrToInt46942 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42221 to i64
%ae42221 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46942)
store volatile %struct.ScmObj* %ae42221, %struct.ScmObj** %stackaddr$makeclosure46941, align 8
%args46379$ae42219$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46943 = alloca %struct.ScmObj*, align 8
%args46379$ae42219$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42221, %struct.ScmObj* %args46379$ae42219$0)
store volatile %struct.ScmObj* %args46379$ae42219$1, %struct.ScmObj** %stackaddr$prim46943, align 8
%stackaddr$prim46944 = alloca %struct.ScmObj*, align 8
%args46379$ae42219$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42220, %struct.ScmObj* %args46379$ae42219$1)
store volatile %struct.ScmObj* %args46379$ae42219$2, %struct.ScmObj** %stackaddr$prim46944, align 8
%clofunc46945 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42219)
musttail call tailcc void %clofunc46945(%struct.ScmObj* %ae42219, %struct.ScmObj* %args46379$ae42219$2)
ret void
}

define tailcc void @proc_clo$ae42219(%struct.ScmObj* %env$ae42219,%struct.ScmObj* %current_45args46192) {
%stackaddr$env-ref46946 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42219, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46946
%stackaddr$prim46947 = alloca %struct.ScmObj*, align 8
%_95k40364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46192)
store volatile %struct.ScmObj* %_95k40364, %struct.ScmObj** %stackaddr$prim46947, align 8
%stackaddr$prim46948 = alloca %struct.ScmObj*, align 8
%current_45args46193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46192)
store volatile %struct.ScmObj* %current_45args46193, %struct.ScmObj** %stackaddr$prim46948, align 8
%stackaddr$prim46949 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46193)
store volatile %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$prim46949, align 8
%stackaddr$makeclosure46950 = alloca %struct.ScmObj*, align 8
%fptrToInt46951 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42243 to i64
%ae42243 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46951)
store volatile %struct.ScmObj* %ae42243, %struct.ScmObj** %stackaddr$makeclosure46950, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42243, %struct.ScmObj* %_37foldl140108, i64 0)
%ae42244 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46952 = alloca %struct.ScmObj*, align 8
%fptrToInt46953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42245 to i64
%ae42245 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46953)
store volatile %struct.ScmObj* %ae42245, %struct.ScmObj** %stackaddr$makeclosure46952, align 8
%args46373$ae42243$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46954 = alloca %struct.ScmObj*, align 8
%args46373$ae42243$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42245, %struct.ScmObj* %args46373$ae42243$0)
store volatile %struct.ScmObj* %args46373$ae42243$1, %struct.ScmObj** %stackaddr$prim46954, align 8
%stackaddr$prim46955 = alloca %struct.ScmObj*, align 8
%args46373$ae42243$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42244, %struct.ScmObj* %args46373$ae42243$1)
store volatile %struct.ScmObj* %args46373$ae42243$2, %struct.ScmObj** %stackaddr$prim46955, align 8
%clofunc46956 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42243)
musttail call tailcc void %clofunc46956(%struct.ScmObj* %ae42243, %struct.ScmObj* %args46373$ae42243$2)
ret void
}

define tailcc void @proc_clo$ae42243(%struct.ScmObj* %env$ae42243,%struct.ScmObj* %current_45args46195) {
%stackaddr$env-ref46957 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42243, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46957
%stackaddr$prim46958 = alloca %struct.ScmObj*, align 8
%_95k40365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46195)
store volatile %struct.ScmObj* %_95k40365, %struct.ScmObj** %stackaddr$prim46958, align 8
%stackaddr$prim46959 = alloca %struct.ScmObj*, align 8
%current_45args46196 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46195)
store volatile %struct.ScmObj* %current_45args46196, %struct.ScmObj** %stackaddr$prim46959, align 8
%stackaddr$prim46960 = alloca %struct.ScmObj*, align 8
%_37_62_6140200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46196)
store volatile %struct.ScmObj* %_37_62_6140200, %struct.ScmObj** %stackaddr$prim46960, align 8
%ae42267 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42268 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46961 = alloca %struct.ScmObj*, align 8
%_37append40196 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42267, %struct.ScmObj* %ae42268)
store volatile %struct.ScmObj* %_37append40196, %struct.ScmObj** %stackaddr$prim46961, align 8
%stackaddr$makeclosure46962 = alloca %struct.ScmObj*, align 8
%fptrToInt46963 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42269 to i64
%ae42269 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt46963)
store volatile %struct.ScmObj* %ae42269, %struct.ScmObj** %stackaddr$makeclosure46962, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42269, %struct.ScmObj* %_37append40196, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42269, %struct.ScmObj* %_37foldl140108, i64 1)
%ae42270 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46964 = alloca %struct.ScmObj*, align 8
%fptrToInt46965 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42271 to i64
%ae42271 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46965)
store volatile %struct.ScmObj* %ae42271, %struct.ScmObj** %stackaddr$makeclosure46964, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42271, %struct.ScmObj* %_37append40196, i64 0)
%args46367$ae42269$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46966 = alloca %struct.ScmObj*, align 8
%args46367$ae42269$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42271, %struct.ScmObj* %args46367$ae42269$0)
store volatile %struct.ScmObj* %args46367$ae42269$1, %struct.ScmObj** %stackaddr$prim46966, align 8
%stackaddr$prim46967 = alloca %struct.ScmObj*, align 8
%args46367$ae42269$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42270, %struct.ScmObj* %args46367$ae42269$1)
store volatile %struct.ScmObj* %args46367$ae42269$2, %struct.ScmObj** %stackaddr$prim46967, align 8
%clofunc46968 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42269)
musttail call tailcc void %clofunc46968(%struct.ScmObj* %ae42269, %struct.ScmObj* %args46367$ae42269$2)
ret void
}

define tailcc void @proc_clo$ae42269(%struct.ScmObj* %env$ae42269,%struct.ScmObj* %current_45args46198) {
%stackaddr$env-ref46969 = alloca %struct.ScmObj*, align 8
%_37append40196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42269, i64 0)
store %struct.ScmObj* %_37append40196, %struct.ScmObj** %stackaddr$env-ref46969
%stackaddr$env-ref46970 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42269, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46970
%stackaddr$prim46971 = alloca %struct.ScmObj*, align 8
%_95k40366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46198)
store volatile %struct.ScmObj* %_95k40366, %struct.ScmObj** %stackaddr$prim46971, align 8
%stackaddr$prim46972 = alloca %struct.ScmObj*, align 8
%current_45args46199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46198)
store volatile %struct.ScmObj* %current_45args46199, %struct.ScmObj** %stackaddr$prim46972, align 8
%stackaddr$prim46973 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46199)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim46973, align 8
%ae42337 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim46974 = alloca %struct.ScmObj*, align 8
%_95040197 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40196, %struct.ScmObj* %ae42337, %struct.ScmObj* %anf_45bind40302)
store volatile %struct.ScmObj* %_95040197, %struct.ScmObj** %stackaddr$prim46974, align 8
%ae42340 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim46975 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40196, %struct.ScmObj* %ae42340)
store volatile %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$prim46975, align 8
%stackaddr$makeclosure46976 = alloca %struct.ScmObj*, align 8
%fptrToInt46977 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42341 to i64
%ae42341 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46977)
store volatile %struct.ScmObj* %ae42341, %struct.ScmObj** %stackaddr$makeclosure46976, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42341, %struct.ScmObj* %_37foldl140108, i64 0)
%ae42342 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46978 = alloca %struct.ScmObj*, align 8
%fptrToInt46979 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42343 to i64
%ae42343 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46979)
store volatile %struct.ScmObj* %ae42343, %struct.ScmObj** %stackaddr$makeclosure46978, align 8
%args46356$ae42341$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46980 = alloca %struct.ScmObj*, align 8
%args46356$ae42341$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42343, %struct.ScmObj* %args46356$ae42341$0)
store volatile %struct.ScmObj* %args46356$ae42341$1, %struct.ScmObj** %stackaddr$prim46980, align 8
%stackaddr$prim46981 = alloca %struct.ScmObj*, align 8
%args46356$ae42341$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42342, %struct.ScmObj* %args46356$ae42341$1)
store volatile %struct.ScmObj* %args46356$ae42341$2, %struct.ScmObj** %stackaddr$prim46981, align 8
%clofunc46982 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42341)
musttail call tailcc void %clofunc46982(%struct.ScmObj* %ae42341, %struct.ScmObj* %args46356$ae42341$2)
ret void
}

define tailcc void @proc_clo$ae42341(%struct.ScmObj* %env$ae42341,%struct.ScmObj* %current_45args46201) {
%stackaddr$env-ref46983 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42341, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46983
%stackaddr$prim46984 = alloca %struct.ScmObj*, align 8
%_95k40367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46201)
store volatile %struct.ScmObj* %_95k40367, %struct.ScmObj** %stackaddr$prim46984, align 8
%stackaddr$prim46985 = alloca %struct.ScmObj*, align 8
%current_45args46202 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46201)
store volatile %struct.ScmObj* %current_45args46202, %struct.ScmObj** %stackaddr$prim46985, align 8
%stackaddr$prim46986 = alloca %struct.ScmObj*, align 8
%_37list_6340188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46202)
store volatile %struct.ScmObj* %_37list_6340188, %struct.ScmObj** %stackaddr$prim46986, align 8
%stackaddr$makeclosure46987 = alloca %struct.ScmObj*, align 8
%fptrToInt46988 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42757 to i64
%ae42757 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46988)
store volatile %struct.ScmObj* %ae42757, %struct.ScmObj** %stackaddr$makeclosure46987, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42757, %struct.ScmObj* %_37foldl140108, i64 0)
%ae42758 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46989 = alloca %struct.ScmObj*, align 8
%fptrToInt46990 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42759 to i64
%ae42759 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46990)
store volatile %struct.ScmObj* %ae42759, %struct.ScmObj** %stackaddr$makeclosure46989, align 8
%args46331$ae42757$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46991 = alloca %struct.ScmObj*, align 8
%args46331$ae42757$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42759, %struct.ScmObj* %args46331$ae42757$0)
store volatile %struct.ScmObj* %args46331$ae42757$1, %struct.ScmObj** %stackaddr$prim46991, align 8
%stackaddr$prim46992 = alloca %struct.ScmObj*, align 8
%args46331$ae42757$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42758, %struct.ScmObj* %args46331$ae42757$1)
store volatile %struct.ScmObj* %args46331$ae42757$2, %struct.ScmObj** %stackaddr$prim46992, align 8
%clofunc46993 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42757)
musttail call tailcc void %clofunc46993(%struct.ScmObj* %ae42757, %struct.ScmObj* %args46331$ae42757$2)
ret void
}

define tailcc void @proc_clo$ae42757(%struct.ScmObj* %env$ae42757,%struct.ScmObj* %current_45args46204) {
%stackaddr$env-ref46994 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42757, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref46994
%stackaddr$prim46995 = alloca %struct.ScmObj*, align 8
%_95k40368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46204)
store volatile %struct.ScmObj* %_95k40368, %struct.ScmObj** %stackaddr$prim46995, align 8
%stackaddr$prim46996 = alloca %struct.ScmObj*, align 8
%current_45args46205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46204)
store volatile %struct.ScmObj* %current_45args46205, %struct.ScmObj** %stackaddr$prim46996, align 8
%stackaddr$prim46997 = alloca %struct.ScmObj*, align 8
%_37drop40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46205)
store volatile %struct.ScmObj* %_37drop40179, %struct.ScmObj** %stackaddr$prim46997, align 8
%stackaddr$makeclosure46998 = alloca %struct.ScmObj*, align 8
%fptrToInt46999 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43293 to i64
%ae43293 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46999)
store volatile %struct.ScmObj* %ae43293, %struct.ScmObj** %stackaddr$makeclosure46998, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43293, %struct.ScmObj* %_37foldl140108, i64 0)
%ae43294 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47000 = alloca %struct.ScmObj*, align 8
%fptrToInt47001 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43295 to i64
%ae43295 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47001)
store volatile %struct.ScmObj* %ae43295, %struct.ScmObj** %stackaddr$makeclosure47000, align 8
%args46307$ae43293$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47002 = alloca %struct.ScmObj*, align 8
%args46307$ae43293$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43295, %struct.ScmObj* %args46307$ae43293$0)
store volatile %struct.ScmObj* %args46307$ae43293$1, %struct.ScmObj** %stackaddr$prim47002, align 8
%stackaddr$prim47003 = alloca %struct.ScmObj*, align 8
%args46307$ae43293$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43294, %struct.ScmObj* %args46307$ae43293$1)
store volatile %struct.ScmObj* %args46307$ae43293$2, %struct.ScmObj** %stackaddr$prim47003, align 8
%clofunc47004 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43293)
musttail call tailcc void %clofunc47004(%struct.ScmObj* %ae43293, %struct.ScmObj* %args46307$ae43293$2)
ret void
}

define tailcc void @proc_clo$ae43293(%struct.ScmObj* %env$ae43293,%struct.ScmObj* %current_45args46207) {
%stackaddr$env-ref47005 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43293, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47005
%stackaddr$prim47006 = alloca %struct.ScmObj*, align 8
%_95k40369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46207)
store volatile %struct.ScmObj* %_95k40369, %struct.ScmObj** %stackaddr$prim47006, align 8
%stackaddr$prim47007 = alloca %struct.ScmObj*, align 8
%current_45args46208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46207)
store volatile %struct.ScmObj* %current_45args46208, %struct.ScmObj** %stackaddr$prim47007, align 8
%stackaddr$prim47008 = alloca %struct.ScmObj*, align 8
%_37memv40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46208)
store volatile %struct.ScmObj* %_37memv40172, %struct.ScmObj** %stackaddr$prim47008, align 8
%stackaddr$makeclosure47009 = alloca %struct.ScmObj*, align 8
%fptrToInt47010 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43697 to i64
%ae43697 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47010)
store volatile %struct.ScmObj* %ae43697, %struct.ScmObj** %stackaddr$makeclosure47009, align 8
%ae43698 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47011 = alloca %struct.ScmObj*, align 8
%fptrToInt47012 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43699 to i64
%ae43699 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47012)
store volatile %struct.ScmObj* %ae43699, %struct.ScmObj** %stackaddr$makeclosure47011, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43699, %struct.ScmObj* %_37foldl140108, i64 0)
%args46281$ae43697$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47013 = alloca %struct.ScmObj*, align 8
%args46281$ae43697$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43699, %struct.ScmObj* %args46281$ae43697$0)
store volatile %struct.ScmObj* %args46281$ae43697$1, %struct.ScmObj** %stackaddr$prim47013, align 8
%stackaddr$prim47014 = alloca %struct.ScmObj*, align 8
%args46281$ae43697$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43698, %struct.ScmObj* %args46281$ae43697$1)
store volatile %struct.ScmObj* %args46281$ae43697$2, %struct.ScmObj** %stackaddr$prim47014, align 8
%clofunc47015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43697)
musttail call tailcc void %clofunc47015(%struct.ScmObj* %ae43697, %struct.ScmObj* %args46281$ae43697$2)
ret void
}

define tailcc void @proc_clo$ae43697(%struct.ScmObj* %env$ae43697,%struct.ScmObj* %current_45args46210) {
%stackaddr$prim47016 = alloca %struct.ScmObj*, align 8
%_95k40370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46210)
store volatile %struct.ScmObj* %_95k40370, %struct.ScmObj** %stackaddr$prim47016, align 8
%stackaddr$prim47017 = alloca %struct.ScmObj*, align 8
%current_45args46211 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46210)
store volatile %struct.ScmObj* %current_45args46211, %struct.ScmObj** %stackaddr$prim47017, align 8
%stackaddr$prim47018 = alloca %struct.ScmObj*, align 8
%_37_4740168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46211)
store volatile %struct.ScmObj* %_37_4740168, %struct.ScmObj** %stackaddr$prim47018, align 8
%stackaddr$makeclosure47019 = alloca %struct.ScmObj*, align 8
%fptrToInt47020 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43795 to i64
%ae43795 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47020)
store volatile %struct.ScmObj* %ae43795, %struct.ScmObj** %stackaddr$makeclosure47019, align 8
%ae43796 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47021 = alloca %struct.ScmObj*, align 8
%fptrToInt47022 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43797 to i64
%ae43797 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47022)
store volatile %struct.ScmObj* %ae43797, %struct.ScmObj** %stackaddr$makeclosure47021, align 8
%args46268$ae43795$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47023 = alloca %struct.ScmObj*, align 8
%args46268$ae43795$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43797, %struct.ScmObj* %args46268$ae43795$0)
store volatile %struct.ScmObj* %args46268$ae43795$1, %struct.ScmObj** %stackaddr$prim47023, align 8
%stackaddr$prim47024 = alloca %struct.ScmObj*, align 8
%args46268$ae43795$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43796, %struct.ScmObj* %args46268$ae43795$1)
store volatile %struct.ScmObj* %args46268$ae43795$2, %struct.ScmObj** %stackaddr$prim47024, align 8
%clofunc47025 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43795)
musttail call tailcc void %clofunc47025(%struct.ScmObj* %ae43795, %struct.ScmObj* %args46268$ae43795$2)
ret void
}

define tailcc void @proc_clo$ae43795(%struct.ScmObj* %env$ae43795,%struct.ScmObj* %current_45args46213) {
%stackaddr$prim47026 = alloca %struct.ScmObj*, align 8
%_95k40371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46213)
store volatile %struct.ScmObj* %_95k40371, %struct.ScmObj** %stackaddr$prim47026, align 8
%stackaddr$prim47027 = alloca %struct.ScmObj*, align 8
%current_45args46214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46213)
store volatile %struct.ScmObj* %current_45args46214, %struct.ScmObj** %stackaddr$prim47027, align 8
%stackaddr$prim47028 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46214)
store volatile %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$prim47028, align 8
%stackaddr$makeclosure47029 = alloca %struct.ScmObj*, align 8
%fptrToInt47030 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43815 to i64
%ae43815 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47030)
store volatile %struct.ScmObj* %ae43815, %struct.ScmObj** %stackaddr$makeclosure47029, align 8
%ae43816 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47031 = alloca %struct.ScmObj*, align 8
%fptrToInt47032 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43817 to i64
%ae43817 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47032)
store volatile %struct.ScmObj* %ae43817, %struct.ScmObj** %stackaddr$makeclosure47031, align 8
%args46263$ae43815$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47033 = alloca %struct.ScmObj*, align 8
%args46263$ae43815$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43817, %struct.ScmObj* %args46263$ae43815$0)
store volatile %struct.ScmObj* %args46263$ae43815$1, %struct.ScmObj** %stackaddr$prim47033, align 8
%stackaddr$prim47034 = alloca %struct.ScmObj*, align 8
%args46263$ae43815$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43816, %struct.ScmObj* %args46263$ae43815$1)
store volatile %struct.ScmObj* %args46263$ae43815$2, %struct.ScmObj** %stackaddr$prim47034, align 8
%clofunc47035 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43815)
musttail call tailcc void %clofunc47035(%struct.ScmObj* %ae43815, %struct.ScmObj* %args46263$ae43815$2)
ret void
}

define tailcc void @proc_clo$ae43815(%struct.ScmObj* %env$ae43815,%struct.ScmObj* %current_45args46216) {
%stackaddr$prim47036 = alloca %struct.ScmObj*, align 8
%_95k40372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46216)
store volatile %struct.ScmObj* %_95k40372, %struct.ScmObj** %stackaddr$prim47036, align 8
%stackaddr$prim47037 = alloca %struct.ScmObj*, align 8
%current_45args46217 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46216)
store volatile %struct.ScmObj* %current_45args46217, %struct.ScmObj** %stackaddr$prim47037, align 8
%stackaddr$prim47038 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46217)
store volatile %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$prim47038, align 8
%stackaddr$makeclosure47039 = alloca %struct.ScmObj*, align 8
%fptrToInt47040 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43837 to i64
%ae43837 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47040)
store volatile %struct.ScmObj* %ae43837, %struct.ScmObj** %stackaddr$makeclosure47039, align 8
%ae43838 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47041 = alloca %struct.ScmObj*, align 8
%fptrToInt47042 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43839 to i64
%ae43839 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47042)
store volatile %struct.ScmObj* %ae43839, %struct.ScmObj** %stackaddr$makeclosure47041, align 8
%args46258$ae43837$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47043 = alloca %struct.ScmObj*, align 8
%args46258$ae43837$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43839, %struct.ScmObj* %args46258$ae43837$0)
store volatile %struct.ScmObj* %args46258$ae43837$1, %struct.ScmObj** %stackaddr$prim47043, align 8
%stackaddr$prim47044 = alloca %struct.ScmObj*, align 8
%args46258$ae43837$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43838, %struct.ScmObj* %args46258$ae43837$1)
store volatile %struct.ScmObj* %args46258$ae43837$2, %struct.ScmObj** %stackaddr$prim47044, align 8
%clofunc47045 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43837)
musttail call tailcc void %clofunc47045(%struct.ScmObj* %ae43837, %struct.ScmObj* %args46258$ae43837$2)
ret void
}

define tailcc void @proc_clo$ae43837(%struct.ScmObj* %env$ae43837,%struct.ScmObj* %current_45args46219) {
%stackaddr$prim47046 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46219)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim47046, align 8
%stackaddr$prim47047 = alloca %struct.ScmObj*, align 8
%current_45args46220 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46219)
store volatile %struct.ScmObj* %current_45args46220, %struct.ScmObj** %stackaddr$prim47047, align 8
%stackaddr$prim47048 = alloca %struct.ScmObj*, align 8
%_37third40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46220)
store volatile %struct.ScmObj* %_37third40162, %struct.ScmObj** %stackaddr$prim47048, align 8
%stackaddr$makeclosure47049 = alloca %struct.ScmObj*, align 8
%fptrToInt47050 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43861 to i64
%ae43861 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47050)
store volatile %struct.ScmObj* %ae43861, %struct.ScmObj** %stackaddr$makeclosure47049, align 8
%ae43862 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47051 = alloca %struct.ScmObj*, align 8
%fptrToInt47052 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43863 to i64
%ae43863 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47052)
store volatile %struct.ScmObj* %ae43863, %struct.ScmObj** %stackaddr$makeclosure47051, align 8
%args46253$ae43861$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47053 = alloca %struct.ScmObj*, align 8
%args46253$ae43861$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43863, %struct.ScmObj* %args46253$ae43861$0)
store volatile %struct.ScmObj* %args46253$ae43861$1, %struct.ScmObj** %stackaddr$prim47053, align 8
%stackaddr$prim47054 = alloca %struct.ScmObj*, align 8
%args46253$ae43861$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43862, %struct.ScmObj* %args46253$ae43861$1)
store volatile %struct.ScmObj* %args46253$ae43861$2, %struct.ScmObj** %stackaddr$prim47054, align 8
%clofunc47055 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43861)
musttail call tailcc void %clofunc47055(%struct.ScmObj* %ae43861, %struct.ScmObj* %args46253$ae43861$2)
ret void
}

define tailcc void @proc_clo$ae43861(%struct.ScmObj* %env$ae43861,%struct.ScmObj* %current_45args46222) {
%stackaddr$prim47056 = alloca %struct.ScmObj*, align 8
%_95k40374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46222)
store volatile %struct.ScmObj* %_95k40374, %struct.ScmObj** %stackaddr$prim47056, align 8
%stackaddr$prim47057 = alloca %struct.ScmObj*, align 8
%current_45args46223 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46222)
store volatile %struct.ScmObj* %current_45args46223, %struct.ScmObj** %stackaddr$prim47057, align 8
%stackaddr$prim47058 = alloca %struct.ScmObj*, align 8
%_37fourth40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46223)
store volatile %struct.ScmObj* %_37fourth40160, %struct.ScmObj** %stackaddr$prim47058, align 8
%stackaddr$makeclosure47059 = alloca %struct.ScmObj*, align 8
%fptrToInt47060 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43887 to i64
%ae43887 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47060)
store volatile %struct.ScmObj* %ae43887, %struct.ScmObj** %stackaddr$makeclosure47059, align 8
%ae43888 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47061 = alloca %struct.ScmObj*, align 8
%fptrToInt47062 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43889 to i64
%ae43889 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47062)
store volatile %struct.ScmObj* %ae43889, %struct.ScmObj** %stackaddr$makeclosure47061, align 8
%args46248$ae43887$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47063 = alloca %struct.ScmObj*, align 8
%args46248$ae43887$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43889, %struct.ScmObj* %args46248$ae43887$0)
store volatile %struct.ScmObj* %args46248$ae43887$1, %struct.ScmObj** %stackaddr$prim47063, align 8
%stackaddr$prim47064 = alloca %struct.ScmObj*, align 8
%args46248$ae43887$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43888, %struct.ScmObj* %args46248$ae43887$1)
store volatile %struct.ScmObj* %args46248$ae43887$2, %struct.ScmObj** %stackaddr$prim47064, align 8
%clofunc47065 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43887)
musttail call tailcc void %clofunc47065(%struct.ScmObj* %ae43887, %struct.ScmObj* %args46248$ae43887$2)
ret void
}

define tailcc void @proc_clo$ae43887(%struct.ScmObj* %env$ae43887,%struct.ScmObj* %current_45args46225) {
%stackaddr$prim47066 = alloca %struct.ScmObj*, align 8
%_95k40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46225)
store volatile %struct.ScmObj* %_95k40375, %struct.ScmObj** %stackaddr$prim47066, align 8
%stackaddr$prim47067 = alloca %struct.ScmObj*, align 8
%current_45args46226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46225)
store volatile %struct.ScmObj* %current_45args46226, %struct.ScmObj** %stackaddr$prim47067, align 8
%stackaddr$prim47068 = alloca %struct.ScmObj*, align 8
%promise_6340221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46226)
store volatile %struct.ScmObj* %promise_6340221, %struct.ScmObj** %stackaddr$prim47068, align 8
%stackaddr$makeclosure47069 = alloca %struct.ScmObj*, align 8
%fptrToInt47070 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43974 to i64
%ae43974 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47070)
store volatile %struct.ScmObj* %ae43974, %struct.ScmObj** %stackaddr$makeclosure47069, align 8
%ae43975 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47071 = alloca %struct.ScmObj*, align 8
%fptrToInt47072 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43976 to i64
%ae43976 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47072)
store volatile %struct.ScmObj* %ae43976, %struct.ScmObj** %stackaddr$makeclosure47071, align 8
%args46241$ae43974$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47073 = alloca %struct.ScmObj*, align 8
%args46241$ae43974$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43976, %struct.ScmObj* %args46241$ae43974$0)
store volatile %struct.ScmObj* %args46241$ae43974$1, %struct.ScmObj** %stackaddr$prim47073, align 8
%stackaddr$prim47074 = alloca %struct.ScmObj*, align 8
%args46241$ae43974$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43975, %struct.ScmObj* %args46241$ae43974$1)
store volatile %struct.ScmObj* %args46241$ae43974$2, %struct.ScmObj** %stackaddr$prim47074, align 8
%clofunc47075 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43974)
musttail call tailcc void %clofunc47075(%struct.ScmObj* %ae43974, %struct.ScmObj* %args46241$ae43974$2)
ret void
}

define tailcc void @proc_clo$ae43974(%struct.ScmObj* %env$ae43974,%struct.ScmObj* %current_45args46228) {
%stackaddr$prim47076 = alloca %struct.ScmObj*, align 8
%_95k40376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46228)
store volatile %struct.ScmObj* %_95k40376, %struct.ScmObj** %stackaddr$prim47076, align 8
%stackaddr$prim47077 = alloca %struct.ScmObj*, align 8
%current_45args46229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46228)
store volatile %struct.ScmObj* %current_45args46229, %struct.ScmObj** %stackaddr$prim47077, align 8
%stackaddr$prim47078 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46229)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim47078, align 8
%stackaddr$makeclosure47079 = alloca %struct.ScmObj*, align 8
%fptrToInt47080 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44015 to i64
%ae44015 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47080)
store volatile %struct.ScmObj* %ae44015, %struct.ScmObj** %stackaddr$makeclosure47079, align 8
%ae44016 = call %struct.ScmObj* @const_init_int(i64 2)
%ae44017 = call %struct.ScmObj* @const_init_int(i64 3)
%ae44018 = call %struct.ScmObj* @const_init_int(i64 4)
%ae44019 = call %struct.ScmObj* @const_init_int(i64 5)
%args46235$anf_45bind40342$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47081 = alloca %struct.ScmObj*, align 8
%args46235$anf_45bind40342$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44019, %struct.ScmObj* %args46235$anf_45bind40342$0)
store volatile %struct.ScmObj* %args46235$anf_45bind40342$1, %struct.ScmObj** %stackaddr$prim47081, align 8
%stackaddr$prim47082 = alloca %struct.ScmObj*, align 8
%args46235$anf_45bind40342$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44018, %struct.ScmObj* %args46235$anf_45bind40342$1)
store volatile %struct.ScmObj* %args46235$anf_45bind40342$2, %struct.ScmObj** %stackaddr$prim47082, align 8
%stackaddr$prim47083 = alloca %struct.ScmObj*, align 8
%args46235$anf_45bind40342$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44017, %struct.ScmObj* %args46235$anf_45bind40342$2)
store volatile %struct.ScmObj* %args46235$anf_45bind40342$3, %struct.ScmObj** %stackaddr$prim47083, align 8
%stackaddr$prim47084 = alloca %struct.ScmObj*, align 8
%args46235$anf_45bind40342$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44016, %struct.ScmObj* %args46235$anf_45bind40342$3)
store volatile %struct.ScmObj* %args46235$anf_45bind40342$4, %struct.ScmObj** %stackaddr$prim47084, align 8
%stackaddr$prim47085 = alloca %struct.ScmObj*, align 8
%args46235$anf_45bind40342$5 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44015, %struct.ScmObj* %args46235$anf_45bind40342$4)
store volatile %struct.ScmObj* %args46235$anf_45bind40342$5, %struct.ScmObj** %stackaddr$prim47085, align 8
%clofunc47086 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40342)
musttail call tailcc void %clofunc47086(%struct.ScmObj* %anf_45bind40342, %struct.ScmObj* %args46235$anf_45bind40342$5)
ret void
}

define tailcc void @proc_clo$ae44015(%struct.ScmObj* %env$ae44015,%struct.ScmObj* %current_45args46231) {
%stackaddr$prim47087 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46231)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim47087, align 8
%stackaddr$prim47088 = alloca %struct.ScmObj*, align 8
%current_45args46232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46231)
store volatile %struct.ScmObj* %current_45args46232, %struct.ScmObj** %stackaddr$prim47088, align 8
%stackaddr$prim47089 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46232)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim47089, align 8
%stackaddr$prim47090 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim47090, align 8
%args46234$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47091 = alloca %struct.ScmObj*, align 8
%args46234$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args46234$k$0)
store volatile %struct.ScmObj* %args46234$k$1, %struct.ScmObj** %stackaddr$prim47091, align 8
%clofunc47092 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc47092(%struct.ScmObj* %k, %struct.ScmObj* %args46234$k$1)
ret void
}

define tailcc void @proc_clo$ae43976(%struct.ScmObj* %env$ae43976,%struct.ScmObj* %t401024022340377) {
%stackaddr$prim47093 = alloca %struct.ScmObj*, align 8
%k40378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t401024022340377)
store volatile %struct.ScmObj* %k40378, %struct.ScmObj** %stackaddr$prim47093, align 8
%stackaddr$prim47094 = alloca %struct.ScmObj*, align 8
%t4010240223 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t401024022340377)
store volatile %struct.ScmObj* %t4010240223, %struct.ScmObj** %stackaddr$prim47094, align 8
%stackaddr$prim47095 = alloca %struct.ScmObj*, align 8
%a40224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t4010240223)
store volatile %struct.ScmObj* %a40224, %struct.ScmObj** %stackaddr$prim47095, align 8
%stackaddr$prim47096 = alloca %struct.ScmObj*, align 8
%t4010240225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t4010240223)
store volatile %struct.ScmObj* %t4010240225, %struct.ScmObj** %stackaddr$prim47096, align 8
%stackaddr$makeclosure47097 = alloca %struct.ScmObj*, align 8
%fptrToInt47098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43981 to i64
%ae43981 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47098)
store volatile %struct.ScmObj* %ae43981, %struct.ScmObj** %stackaddr$makeclosure47097, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43981, %struct.ScmObj* %k40378, i64 0)
%ae43982 = call %struct.ScmObj* @const_init_int(i64 0)
%args46240$ae43981$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47099 = alloca %struct.ScmObj*, align 8
%args46240$ae43981$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %t4010240225, %struct.ScmObj* %args46240$ae43981$0)
store volatile %struct.ScmObj* %args46240$ae43981$1, %struct.ScmObj** %stackaddr$prim47099, align 8
%stackaddr$prim47100 = alloca %struct.ScmObj*, align 8
%args46240$ae43981$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43982, %struct.ScmObj* %args46240$ae43981$1)
store volatile %struct.ScmObj* %args46240$ae43981$2, %struct.ScmObj** %stackaddr$prim47100, align 8
%clofunc47101 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43981)
musttail call tailcc void %clofunc47101(%struct.ScmObj* %ae43981, %struct.ScmObj* %args46240$ae43981$2)
ret void
}

define tailcc void @proc_clo$ae43981(%struct.ScmObj* %env$ae43981,%struct.ScmObj* %current_45args46236) {
%stackaddr$env-ref47102 = alloca %struct.ScmObj*, align 8
%k40378 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43981, i64 0)
store %struct.ScmObj* %k40378, %struct.ScmObj** %stackaddr$env-ref47102
%stackaddr$prim47103 = alloca %struct.ScmObj*, align 8
%_95k40379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46236)
store volatile %struct.ScmObj* %_95k40379, %struct.ScmObj** %stackaddr$prim47103, align 8
%stackaddr$prim47104 = alloca %struct.ScmObj*, align 8
%current_45args46237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46236)
store volatile %struct.ScmObj* %current_45args46237, %struct.ScmObj** %stackaddr$prim47104, align 8
%stackaddr$prim47105 = alloca %struct.ScmObj*, align 8
%b40226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46237)
store volatile %struct.ScmObj* %b40226, %struct.ScmObj** %stackaddr$prim47105, align 8
%stackaddr$prim47106 = alloca %struct.ScmObj*, align 8
%cpsprim40380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %b40226)
store volatile %struct.ScmObj* %cpsprim40380, %struct.ScmObj** %stackaddr$prim47106, align 8
%ae43989 = call %struct.ScmObj* @const_init_int(i64 0)
%args46239$k40378$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47107 = alloca %struct.ScmObj*, align 8
%args46239$k40378$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40380, %struct.ScmObj* %args46239$k40378$0)
store volatile %struct.ScmObj* %args46239$k40378$1, %struct.ScmObj** %stackaddr$prim47107, align 8
%stackaddr$prim47108 = alloca %struct.ScmObj*, align 8
%args46239$k40378$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43989, %struct.ScmObj* %args46239$k40378$1)
store volatile %struct.ScmObj* %args46239$k40378$2, %struct.ScmObj** %stackaddr$prim47108, align 8
%clofunc47109 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40378)
musttail call tailcc void %clofunc47109(%struct.ScmObj* %k40378, %struct.ScmObj* %args46239$k40378$2)
ret void
}

define tailcc void @proc_clo$ae43889(%struct.ScmObj* %env$ae43889,%struct.ScmObj* %current_45args46242) {
%stackaddr$prim47110 = alloca %struct.ScmObj*, align 8
%k40381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46242)
store volatile %struct.ScmObj* %k40381, %struct.ScmObj** %stackaddr$prim47110, align 8
%stackaddr$prim47111 = alloca %struct.ScmObj*, align 8
%current_45args46243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46242)
store volatile %struct.ScmObj* %current_45args46243, %struct.ScmObj** %stackaddr$prim47111, align 8
%stackaddr$prim47112 = alloca %struct.ScmObj*, align 8
%thunk40222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46243)
store volatile %struct.ScmObj* %thunk40222, %struct.ScmObj** %stackaddr$prim47112, align 8
%stackaddr$prim47113 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40222)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim47113, align 8
%truthy$cmp47114 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40338)
%cmp$cmp47114 = icmp eq i64 %truthy$cmp47114, 1
br i1 %cmp$cmp47114, label %truebranch$cmp47114, label %falsebranch$cmp47114
truebranch$cmp47114:
%stackaddr$prim47115 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40222)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim47115, align 8
%ae43894 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim47116 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40339, %struct.ScmObj* %ae43894)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim47116, align 8
%truthy$cmp47117 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40340)
%cmp$cmp47117 = icmp eq i64 %truthy$cmp47117, 1
br i1 %cmp$cmp47117, label %truebranch$cmp47117, label %falsebranch$cmp47117
truebranch$cmp47117:
%ae43897 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47118 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40222, %struct.ScmObj* %ae43897)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim47118, align 8
%ae43899 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4389947119, i32 0, i32 0))
%stackaddr$prim47120 = alloca %struct.ScmObj*, align 8
%cpsprim40382 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40341, %struct.ScmObj* %ae43899)
store volatile %struct.ScmObj* %cpsprim40382, %struct.ScmObj** %stackaddr$prim47120, align 8
%ae43901 = call %struct.ScmObj* @const_init_int(i64 0)
%args46245$k40381$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47121 = alloca %struct.ScmObj*, align 8
%args46245$k40381$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40382, %struct.ScmObj* %args46245$k40381$0)
store volatile %struct.ScmObj* %args46245$k40381$1, %struct.ScmObj** %stackaddr$prim47121, align 8
%stackaddr$prim47122 = alloca %struct.ScmObj*, align 8
%args46245$k40381$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43901, %struct.ScmObj* %args46245$k40381$1)
store volatile %struct.ScmObj* %args46245$k40381$2, %struct.ScmObj** %stackaddr$prim47122, align 8
%clofunc47123 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40381)
musttail call tailcc void %clofunc47123(%struct.ScmObj* %k40381, %struct.ScmObj* %args46245$k40381$2)
ret void
falsebranch$cmp47117:
%ae43919 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43920 = call %struct.ScmObj* @const_init_false()
%args46246$k40381$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47124 = alloca %struct.ScmObj*, align 8
%args46246$k40381$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43920, %struct.ScmObj* %args46246$k40381$0)
store volatile %struct.ScmObj* %args46246$k40381$1, %struct.ScmObj** %stackaddr$prim47124, align 8
%stackaddr$prim47125 = alloca %struct.ScmObj*, align 8
%args46246$k40381$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43919, %struct.ScmObj* %args46246$k40381$1)
store volatile %struct.ScmObj* %args46246$k40381$2, %struct.ScmObj** %stackaddr$prim47125, align 8
%clofunc47126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40381)
musttail call tailcc void %clofunc47126(%struct.ScmObj* %k40381, %struct.ScmObj* %args46246$k40381$2)
ret void
falsebranch$cmp47114:
%ae43941 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43942 = call %struct.ScmObj* @const_init_false()
%args46247$k40381$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47127 = alloca %struct.ScmObj*, align 8
%args46247$k40381$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43942, %struct.ScmObj* %args46247$k40381$0)
store volatile %struct.ScmObj* %args46247$k40381$1, %struct.ScmObj** %stackaddr$prim47127, align 8
%stackaddr$prim47128 = alloca %struct.ScmObj*, align 8
%args46247$k40381$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43941, %struct.ScmObj* %args46247$k40381$1)
store volatile %struct.ScmObj* %args46247$k40381$2, %struct.ScmObj** %stackaddr$prim47128, align 8
%clofunc47129 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40381)
musttail call tailcc void %clofunc47129(%struct.ScmObj* %k40381, %struct.ScmObj* %args46247$k40381$2)
ret void
}

define tailcc void @proc_clo$ae43863(%struct.ScmObj* %env$ae43863,%struct.ScmObj* %current_45args46249) {
%stackaddr$prim47130 = alloca %struct.ScmObj*, align 8
%k40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46249)
store volatile %struct.ScmObj* %k40383, %struct.ScmObj** %stackaddr$prim47130, align 8
%stackaddr$prim47131 = alloca %struct.ScmObj*, align 8
%current_45args46250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46249)
store volatile %struct.ScmObj* %current_45args46250, %struct.ScmObj** %stackaddr$prim47131, align 8
%stackaddr$prim47132 = alloca %struct.ScmObj*, align 8
%x40161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46250)
store volatile %struct.ScmObj* %x40161, %struct.ScmObj** %stackaddr$prim47132, align 8
%stackaddr$prim47133 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40161)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim47133, align 8
%stackaddr$prim47134 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40335)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim47134, align 8
%stackaddr$prim47135 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40336)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim47135, align 8
%stackaddr$prim47136 = alloca %struct.ScmObj*, align 8
%cpsprim40384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40337)
store volatile %struct.ScmObj* %cpsprim40384, %struct.ScmObj** %stackaddr$prim47136, align 8
%ae43869 = call %struct.ScmObj* @const_init_int(i64 0)
%args46252$k40383$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47137 = alloca %struct.ScmObj*, align 8
%args46252$k40383$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40384, %struct.ScmObj* %args46252$k40383$0)
store volatile %struct.ScmObj* %args46252$k40383$1, %struct.ScmObj** %stackaddr$prim47137, align 8
%stackaddr$prim47138 = alloca %struct.ScmObj*, align 8
%args46252$k40383$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43869, %struct.ScmObj* %args46252$k40383$1)
store volatile %struct.ScmObj* %args46252$k40383$2, %struct.ScmObj** %stackaddr$prim47138, align 8
%clofunc47139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40383)
musttail call tailcc void %clofunc47139(%struct.ScmObj* %k40383, %struct.ScmObj* %args46252$k40383$2)
ret void
}

define tailcc void @proc_clo$ae43839(%struct.ScmObj* %env$ae43839,%struct.ScmObj* %current_45args46254) {
%stackaddr$prim47140 = alloca %struct.ScmObj*, align 8
%k40385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46254)
store volatile %struct.ScmObj* %k40385, %struct.ScmObj** %stackaddr$prim47140, align 8
%stackaddr$prim47141 = alloca %struct.ScmObj*, align 8
%current_45args46255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46254)
store volatile %struct.ScmObj* %current_45args46255, %struct.ScmObj** %stackaddr$prim47141, align 8
%stackaddr$prim47142 = alloca %struct.ScmObj*, align 8
%x40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46255)
store volatile %struct.ScmObj* %x40163, %struct.ScmObj** %stackaddr$prim47142, align 8
%stackaddr$prim47143 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40163)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim47143, align 8
%stackaddr$prim47144 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40333)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim47144, align 8
%stackaddr$prim47145 = alloca %struct.ScmObj*, align 8
%cpsprim40386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %cpsprim40386, %struct.ScmObj** %stackaddr$prim47145, align 8
%ae43844 = call %struct.ScmObj* @const_init_int(i64 0)
%args46257$k40385$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47146 = alloca %struct.ScmObj*, align 8
%args46257$k40385$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40386, %struct.ScmObj* %args46257$k40385$0)
store volatile %struct.ScmObj* %args46257$k40385$1, %struct.ScmObj** %stackaddr$prim47146, align 8
%stackaddr$prim47147 = alloca %struct.ScmObj*, align 8
%args46257$k40385$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43844, %struct.ScmObj* %args46257$k40385$1)
store volatile %struct.ScmObj* %args46257$k40385$2, %struct.ScmObj** %stackaddr$prim47147, align 8
%clofunc47148 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40385)
musttail call tailcc void %clofunc47148(%struct.ScmObj* %k40385, %struct.ScmObj* %args46257$k40385$2)
ret void
}

define tailcc void @proc_clo$ae43817(%struct.ScmObj* %env$ae43817,%struct.ScmObj* %current_45args46259) {
%stackaddr$prim47149 = alloca %struct.ScmObj*, align 8
%k40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46259)
store volatile %struct.ScmObj* %k40387, %struct.ScmObj** %stackaddr$prim47149, align 8
%stackaddr$prim47150 = alloca %struct.ScmObj*, align 8
%current_45args46260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46259)
store volatile %struct.ScmObj* %current_45args46260, %struct.ScmObj** %stackaddr$prim47150, align 8
%stackaddr$prim47151 = alloca %struct.ScmObj*, align 8
%x40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46260)
store volatile %struct.ScmObj* %x40165, %struct.ScmObj** %stackaddr$prim47151, align 8
%stackaddr$prim47152 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40165)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim47152, align 8
%stackaddr$prim47153 = alloca %struct.ScmObj*, align 8
%cpsprim40388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40332)
store volatile %struct.ScmObj* %cpsprim40388, %struct.ScmObj** %stackaddr$prim47153, align 8
%ae43821 = call %struct.ScmObj* @const_init_int(i64 0)
%args46262$k40387$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47154 = alloca %struct.ScmObj*, align 8
%args46262$k40387$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40388, %struct.ScmObj* %args46262$k40387$0)
store volatile %struct.ScmObj* %args46262$k40387$1, %struct.ScmObj** %stackaddr$prim47154, align 8
%stackaddr$prim47155 = alloca %struct.ScmObj*, align 8
%args46262$k40387$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43821, %struct.ScmObj* %args46262$k40387$1)
store volatile %struct.ScmObj* %args46262$k40387$2, %struct.ScmObj** %stackaddr$prim47155, align 8
%clofunc47156 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40387)
musttail call tailcc void %clofunc47156(%struct.ScmObj* %k40387, %struct.ScmObj* %args46262$k40387$2)
ret void
}

define tailcc void @proc_clo$ae43797(%struct.ScmObj* %env$ae43797,%struct.ScmObj* %current_45args46264) {
%stackaddr$prim47157 = alloca %struct.ScmObj*, align 8
%k40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46264)
store volatile %struct.ScmObj* %k40389, %struct.ScmObj** %stackaddr$prim47157, align 8
%stackaddr$prim47158 = alloca %struct.ScmObj*, align 8
%current_45args46265 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46264)
store volatile %struct.ScmObj* %current_45args46265, %struct.ScmObj** %stackaddr$prim47158, align 8
%stackaddr$prim47159 = alloca %struct.ScmObj*, align 8
%x40167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46265)
store volatile %struct.ScmObj* %x40167, %struct.ScmObj** %stackaddr$prim47159, align 8
%stackaddr$prim47160 = alloca %struct.ScmObj*, align 8
%cpsprim40390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40167)
store volatile %struct.ScmObj* %cpsprim40390, %struct.ScmObj** %stackaddr$prim47160, align 8
%ae43800 = call %struct.ScmObj* @const_init_int(i64 0)
%args46267$k40389$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47161 = alloca %struct.ScmObj*, align 8
%args46267$k40389$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40390, %struct.ScmObj* %args46267$k40389$0)
store volatile %struct.ScmObj* %args46267$k40389$1, %struct.ScmObj** %stackaddr$prim47161, align 8
%stackaddr$prim47162 = alloca %struct.ScmObj*, align 8
%args46267$k40389$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43800, %struct.ScmObj* %args46267$k40389$1)
store volatile %struct.ScmObj* %args46267$k40389$2, %struct.ScmObj** %stackaddr$prim47162, align 8
%clofunc47163 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40389)
musttail call tailcc void %clofunc47163(%struct.ScmObj* %k40389, %struct.ScmObj* %args46267$k40389$2)
ret void
}

define tailcc void @proc_clo$ae43699(%struct.ScmObj* %env$ae43699,%struct.ScmObj* %args4016940391) {
%stackaddr$env-ref47164 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43699, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47164
%stackaddr$prim47165 = alloca %struct.ScmObj*, align 8
%k40392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4016940391)
store volatile %struct.ScmObj* %k40392, %struct.ScmObj** %stackaddr$prim47165, align 8
%stackaddr$prim47166 = alloca %struct.ScmObj*, align 8
%args40169 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4016940391)
store volatile %struct.ScmObj* %args40169, %struct.ScmObj** %stackaddr$prim47166, align 8
%stackaddr$prim47167 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim47167, align 8
%truthy$cmp47168 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40326)
%cmp$cmp47168 = icmp eq i64 %truthy$cmp47168, 1
br i1 %cmp$cmp47168, label %truebranch$cmp47168, label %falsebranch$cmp47168
truebranch$cmp47168:
%ae43705 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43706 = call %struct.ScmObj* @const_init_int(i64 1)
%args46269$k40392$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47169 = alloca %struct.ScmObj*, align 8
%args46269$k40392$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43706, %struct.ScmObj* %args46269$k40392$0)
store volatile %struct.ScmObj* %args46269$k40392$1, %struct.ScmObj** %stackaddr$prim47169, align 8
%stackaddr$prim47170 = alloca %struct.ScmObj*, align 8
%args46269$k40392$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43705, %struct.ScmObj* %args46269$k40392$1)
store volatile %struct.ScmObj* %args46269$k40392$2, %struct.ScmObj** %stackaddr$prim47170, align 8
%clofunc47171 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40392)
musttail call tailcc void %clofunc47171(%struct.ScmObj* %k40392, %struct.ScmObj* %args46269$k40392$2)
ret void
falsebranch$cmp47168:
%stackaddr$prim47172 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim47172, align 8
%stackaddr$prim47173 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim47173, align 8
%truthy$cmp47174 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40328)
%cmp$cmp47174 = icmp eq i64 %truthy$cmp47174, 1
br i1 %cmp$cmp47174, label %truebranch$cmp47174, label %falsebranch$cmp47174
truebranch$cmp47174:
%stackaddr$prim47175 = alloca %struct.ScmObj*, align 8
%cpsprim40393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %cpsprim40393, %struct.ScmObj** %stackaddr$prim47175, align 8
%ae43718 = call %struct.ScmObj* @const_init_int(i64 0)
%args46270$k40392$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47176 = alloca %struct.ScmObj*, align 8
%args46270$k40392$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40393, %struct.ScmObj* %args46270$k40392$0)
store volatile %struct.ScmObj* %args46270$k40392$1, %struct.ScmObj** %stackaddr$prim47176, align 8
%stackaddr$prim47177 = alloca %struct.ScmObj*, align 8
%args46270$k40392$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43718, %struct.ScmObj* %args46270$k40392$1)
store volatile %struct.ScmObj* %args46270$k40392$2, %struct.ScmObj** %stackaddr$prim47177, align 8
%clofunc47178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40392)
musttail call tailcc void %clofunc47178(%struct.ScmObj* %k40392, %struct.ScmObj* %args46270$k40392$2)
ret void
falsebranch$cmp47174:
%stackaddr$makeclosure47179 = alloca %struct.ScmObj*, align 8
%fptrToInt47180 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43723 to i64
%ae43723 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47180)
store volatile %struct.ScmObj* %ae43723, %struct.ScmObj** %stackaddr$makeclosure47179, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43723, %struct.ScmObj* %args40169, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43723, %struct.ScmObj* %k40392, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43723, %struct.ScmObj* %_37foldl140108, i64 2)
%ae43724 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47181 = alloca %struct.ScmObj*, align 8
%fptrToInt47182 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43725 to i64
%ae43725 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47182)
store volatile %struct.ScmObj* %ae43725, %struct.ScmObj** %stackaddr$makeclosure47181, align 8
%args46280$ae43723$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47183 = alloca %struct.ScmObj*, align 8
%args46280$ae43723$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43725, %struct.ScmObj* %args46280$ae43723$0)
store volatile %struct.ScmObj* %args46280$ae43723$1, %struct.ScmObj** %stackaddr$prim47183, align 8
%stackaddr$prim47184 = alloca %struct.ScmObj*, align 8
%args46280$ae43723$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43724, %struct.ScmObj* %args46280$ae43723$1)
store volatile %struct.ScmObj* %args46280$ae43723$2, %struct.ScmObj** %stackaddr$prim47184, align 8
%clofunc47185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43723)
musttail call tailcc void %clofunc47185(%struct.ScmObj* %ae43723, %struct.ScmObj* %args46280$ae43723$2)
ret void
}

define tailcc void @proc_clo$ae43723(%struct.ScmObj* %env$ae43723,%struct.ScmObj* %current_45args46271) {
%stackaddr$env-ref47186 = alloca %struct.ScmObj*, align 8
%args40169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43723, i64 0)
store %struct.ScmObj* %args40169, %struct.ScmObj** %stackaddr$env-ref47186
%stackaddr$env-ref47187 = alloca %struct.ScmObj*, align 8
%k40392 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43723, i64 1)
store %struct.ScmObj* %k40392, %struct.ScmObj** %stackaddr$env-ref47187
%stackaddr$env-ref47188 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43723, i64 2)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47188
%stackaddr$prim47189 = alloca %struct.ScmObj*, align 8
%_95k40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46271)
store volatile %struct.ScmObj* %_95k40394, %struct.ScmObj** %stackaddr$prim47189, align 8
%stackaddr$prim47190 = alloca %struct.ScmObj*, align 8
%current_45args46272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46271)
store volatile %struct.ScmObj* %current_45args46272, %struct.ScmObj** %stackaddr$prim47190, align 8
%stackaddr$prim47191 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46272)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim47191, align 8
%stackaddr$prim47192 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim47192, align 8
%stackaddr$prim47193 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim47193, align 8
%args46274$_37foldl140108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47194 = alloca %struct.ScmObj*, align 8
%args46274$_37foldl140108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40331, %struct.ScmObj* %args46274$_37foldl140108$0)
store volatile %struct.ScmObj* %args46274$_37foldl140108$1, %struct.ScmObj** %stackaddr$prim47194, align 8
%stackaddr$prim47195 = alloca %struct.ScmObj*, align 8
%args46274$_37foldl140108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40330, %struct.ScmObj* %args46274$_37foldl140108$1)
store volatile %struct.ScmObj* %args46274$_37foldl140108$2, %struct.ScmObj** %stackaddr$prim47195, align 8
%stackaddr$prim47196 = alloca %struct.ScmObj*, align 8
%args46274$_37foldl140108$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40329, %struct.ScmObj* %args46274$_37foldl140108$2)
store volatile %struct.ScmObj* %args46274$_37foldl140108$3, %struct.ScmObj** %stackaddr$prim47196, align 8
%stackaddr$prim47197 = alloca %struct.ScmObj*, align 8
%args46274$_37foldl140108$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40392, %struct.ScmObj* %args46274$_37foldl140108$3)
store volatile %struct.ScmObj* %args46274$_37foldl140108$4, %struct.ScmObj** %stackaddr$prim47197, align 8
%clofunc47198 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140108)
musttail call tailcc void %clofunc47198(%struct.ScmObj* %_37foldl140108, %struct.ScmObj* %args46274$_37foldl140108$4)
ret void
}

define tailcc void @proc_clo$ae43725(%struct.ScmObj* %env$ae43725,%struct.ScmObj* %current_45args46275) {
%stackaddr$prim47199 = alloca %struct.ScmObj*, align 8
%k40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46275)
store volatile %struct.ScmObj* %k40395, %struct.ScmObj** %stackaddr$prim47199, align 8
%stackaddr$prim47200 = alloca %struct.ScmObj*, align 8
%current_45args46276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46275)
store volatile %struct.ScmObj* %current_45args46276, %struct.ScmObj** %stackaddr$prim47200, align 8
%stackaddr$prim47201 = alloca %struct.ScmObj*, align 8
%n40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46276)
store volatile %struct.ScmObj* %n40171, %struct.ScmObj** %stackaddr$prim47201, align 8
%stackaddr$prim47202 = alloca %struct.ScmObj*, align 8
%current_45args46277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46276)
store volatile %struct.ScmObj* %current_45args46277, %struct.ScmObj** %stackaddr$prim47202, align 8
%stackaddr$prim47203 = alloca %struct.ScmObj*, align 8
%v40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46277)
store volatile %struct.ScmObj* %v40170, %struct.ScmObj** %stackaddr$prim47203, align 8
%stackaddr$prim47204 = alloca %struct.ScmObj*, align 8
%cpsprim40396 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40170, %struct.ScmObj* %n40171)
store volatile %struct.ScmObj* %cpsprim40396, %struct.ScmObj** %stackaddr$prim47204, align 8
%ae43729 = call %struct.ScmObj* @const_init_int(i64 0)
%args46279$k40395$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47205 = alloca %struct.ScmObj*, align 8
%args46279$k40395$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40396, %struct.ScmObj* %args46279$k40395$0)
store volatile %struct.ScmObj* %args46279$k40395$1, %struct.ScmObj** %stackaddr$prim47205, align 8
%stackaddr$prim47206 = alloca %struct.ScmObj*, align 8
%args46279$k40395$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43729, %struct.ScmObj* %args46279$k40395$1)
store volatile %struct.ScmObj* %args46279$k40395$2, %struct.ScmObj** %stackaddr$prim47206, align 8
%clofunc47207 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40395)
musttail call tailcc void %clofunc47207(%struct.ScmObj* %k40395, %struct.ScmObj* %args46279$k40395$2)
ret void
}

define tailcc void @proc_clo$ae43295(%struct.ScmObj* %env$ae43295,%struct.ScmObj* %current_45args46282) {
%stackaddr$prim47208 = alloca %struct.ScmObj*, align 8
%k40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46282)
store volatile %struct.ScmObj* %k40397, %struct.ScmObj** %stackaddr$prim47208, align 8
%stackaddr$prim47209 = alloca %struct.ScmObj*, align 8
%current_45args46283 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46282)
store volatile %struct.ScmObj* %current_45args46283, %struct.ScmObj** %stackaddr$prim47209, align 8
%stackaddr$prim47210 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46283)
store volatile %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$prim47210, align 8
%stackaddr$prim47211 = alloca %struct.ScmObj*, align 8
%current_45args46284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46283)
store volatile %struct.ScmObj* %current_45args46284, %struct.ScmObj** %stackaddr$prim47211, align 8
%stackaddr$prim47212 = alloca %struct.ScmObj*, align 8
%lst40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46284)
store volatile %struct.ScmObj* %lst40173, %struct.ScmObj** %stackaddr$prim47212, align 8
%ae43296 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47213 = alloca %struct.ScmObj*, align 8
%lst40175 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43296, %struct.ScmObj* %lst40173)
store volatile %struct.ScmObj* %lst40175, %struct.ScmObj** %stackaddr$prim47213, align 8
%stackaddr$makeclosure47214 = alloca %struct.ScmObj*, align 8
%fptrToInt47215 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43298 to i64
%ae43298 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47215)
store volatile %struct.ScmObj* %ae43298, %struct.ScmObj** %stackaddr$makeclosure47214, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43298, %struct.ScmObj* %lst40175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43298, %struct.ScmObj* %v40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43298, %struct.ScmObj* %k40397, i64 2)
%ae43299 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47216 = alloca %struct.ScmObj*, align 8
%fptrToInt47217 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43300 to i64
%ae43300 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47217)
store volatile %struct.ScmObj* %ae43300, %struct.ScmObj** %stackaddr$makeclosure47216, align 8
%args46306$ae43298$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47218 = alloca %struct.ScmObj*, align 8
%args46306$ae43298$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43300, %struct.ScmObj* %args46306$ae43298$0)
store volatile %struct.ScmObj* %args46306$ae43298$1, %struct.ScmObj** %stackaddr$prim47218, align 8
%stackaddr$prim47219 = alloca %struct.ScmObj*, align 8
%args46306$ae43298$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43299, %struct.ScmObj* %args46306$ae43298$1)
store volatile %struct.ScmObj* %args46306$ae43298$2, %struct.ScmObj** %stackaddr$prim47219, align 8
%clofunc47220 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43298)
musttail call tailcc void %clofunc47220(%struct.ScmObj* %ae43298, %struct.ScmObj* %args46306$ae43298$2)
ret void
}

define tailcc void @proc_clo$ae43298(%struct.ScmObj* %env$ae43298,%struct.ScmObj* %current_45args46286) {
%stackaddr$env-ref47221 = alloca %struct.ScmObj*, align 8
%lst40175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43298, i64 0)
store %struct.ScmObj* %lst40175, %struct.ScmObj** %stackaddr$env-ref47221
%stackaddr$env-ref47222 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43298, i64 1)
store %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$env-ref47222
%stackaddr$env-ref47223 = alloca %struct.ScmObj*, align 8
%k40397 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43298, i64 2)
store %struct.ScmObj* %k40397, %struct.ScmObj** %stackaddr$env-ref47223
%stackaddr$prim47224 = alloca %struct.ScmObj*, align 8
%_95k40398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46286)
store volatile %struct.ScmObj* %_95k40398, %struct.ScmObj** %stackaddr$prim47224, align 8
%stackaddr$prim47225 = alloca %struct.ScmObj*, align 8
%current_45args46287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46286)
store volatile %struct.ScmObj* %current_45args46287, %struct.ScmObj** %stackaddr$prim47225, align 8
%stackaddr$prim47226 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46287)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim47226, align 8
%stackaddr$makeclosure47227 = alloca %struct.ScmObj*, align 8
%fptrToInt47228 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43314 to i64
%ae43314 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47228)
store volatile %struct.ScmObj* %ae43314, %struct.ScmObj** %stackaddr$makeclosure47227, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43314, %struct.ScmObj* %lst40175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43314, %struct.ScmObj* %v40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43314, %struct.ScmObj* %k40397, i64 2)
%stackaddr$makeclosure47229 = alloca %struct.ScmObj*, align 8
%fptrToInt47230 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43315 to i64
%ae43315 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47230)
store volatile %struct.ScmObj* %ae43315, %struct.ScmObj** %stackaddr$makeclosure47229, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43315, %struct.ScmObj* %lst40175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43315, %struct.ScmObj* %v40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43315, %struct.ScmObj* %k40397, i64 2)
%args46301$anf_45bind40318$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47231 = alloca %struct.ScmObj*, align 8
%args46301$anf_45bind40318$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43315, %struct.ScmObj* %args46301$anf_45bind40318$0)
store volatile %struct.ScmObj* %args46301$anf_45bind40318$1, %struct.ScmObj** %stackaddr$prim47231, align 8
%stackaddr$prim47232 = alloca %struct.ScmObj*, align 8
%args46301$anf_45bind40318$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43314, %struct.ScmObj* %args46301$anf_45bind40318$1)
store volatile %struct.ScmObj* %args46301$anf_45bind40318$2, %struct.ScmObj** %stackaddr$prim47232, align 8
%clofunc47233 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40318)
musttail call tailcc void %clofunc47233(%struct.ScmObj* %anf_45bind40318, %struct.ScmObj* %args46301$anf_45bind40318$2)
ret void
}

define tailcc void @proc_clo$ae43314(%struct.ScmObj* %env$ae43314,%struct.ScmObj* %current_45args46289) {
%stackaddr$env-ref47234 = alloca %struct.ScmObj*, align 8
%lst40175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43314, i64 0)
store %struct.ScmObj* %lst40175, %struct.ScmObj** %stackaddr$env-ref47234
%stackaddr$env-ref47235 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43314, i64 1)
store %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$env-ref47235
%stackaddr$env-ref47236 = alloca %struct.ScmObj*, align 8
%k40397 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43314, i64 2)
store %struct.ScmObj* %k40397, %struct.ScmObj** %stackaddr$env-ref47236
%stackaddr$prim47237 = alloca %struct.ScmObj*, align 8
%_95k40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46289)
store volatile %struct.ScmObj* %_95k40399, %struct.ScmObj** %stackaddr$prim47237, align 8
%stackaddr$prim47238 = alloca %struct.ScmObj*, align 8
%current_45args46290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46289)
store volatile %struct.ScmObj* %current_45args46290, %struct.ScmObj** %stackaddr$prim47238, align 8
%stackaddr$prim47239 = alloca %struct.ScmObj*, align 8
%cc40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46290)
store volatile %struct.ScmObj* %cc40176, %struct.ScmObj** %stackaddr$prim47239, align 8
%ae43423 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47240 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43423)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim47240, align 8
%stackaddr$prim47241 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim47241, align 8
%truthy$cmp47242 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40320)
%cmp$cmp47242 = icmp eq i64 %truthy$cmp47242, 1
br i1 %cmp$cmp47242, label %truebranch$cmp47242, label %falsebranch$cmp47242
truebranch$cmp47242:
%ae43427 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43428 = call %struct.ScmObj* @const_init_false()
%args46292$k40397$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47243 = alloca %struct.ScmObj*, align 8
%args46292$k40397$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43428, %struct.ScmObj* %args46292$k40397$0)
store volatile %struct.ScmObj* %args46292$k40397$1, %struct.ScmObj** %stackaddr$prim47243, align 8
%stackaddr$prim47244 = alloca %struct.ScmObj*, align 8
%args46292$k40397$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43427, %struct.ScmObj* %args46292$k40397$1)
store volatile %struct.ScmObj* %args46292$k40397$2, %struct.ScmObj** %stackaddr$prim47244, align 8
%clofunc47245 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40397)
musttail call tailcc void %clofunc47245(%struct.ScmObj* %k40397, %struct.ScmObj* %args46292$k40397$2)
ret void
falsebranch$cmp47242:
%ae43436 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47246 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43436)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim47246, align 8
%stackaddr$prim47247 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim47247, align 8
%stackaddr$prim47248 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40322, %struct.ScmObj* %v40174)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim47248, align 8
%truthy$cmp47249 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40323)
%cmp$cmp47249 = icmp eq i64 %truthy$cmp47249, 1
br i1 %cmp$cmp47249, label %truebranch$cmp47249, label %falsebranch$cmp47249
truebranch$cmp47249:
%ae43442 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47250 = alloca %struct.ScmObj*, align 8
%cpsprim40400 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43442)
store volatile %struct.ScmObj* %cpsprim40400, %struct.ScmObj** %stackaddr$prim47250, align 8
%ae43444 = call %struct.ScmObj* @const_init_int(i64 0)
%args46293$k40397$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47251 = alloca %struct.ScmObj*, align 8
%args46293$k40397$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40400, %struct.ScmObj* %args46293$k40397$0)
store volatile %struct.ScmObj* %args46293$k40397$1, %struct.ScmObj** %stackaddr$prim47251, align 8
%stackaddr$prim47252 = alloca %struct.ScmObj*, align 8
%args46293$k40397$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43444, %struct.ScmObj* %args46293$k40397$1)
store volatile %struct.ScmObj* %args46293$k40397$2, %struct.ScmObj** %stackaddr$prim47252, align 8
%clofunc47253 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40397)
musttail call tailcc void %clofunc47253(%struct.ScmObj* %k40397, %struct.ScmObj* %args46293$k40397$2)
ret void
falsebranch$cmp47249:
%ae43455 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47254 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43455)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim47254, align 8
%stackaddr$prim47255 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim47255, align 8
%ae43458 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47256 = alloca %struct.ScmObj*, align 8
%_95040178 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43458, %struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %_95040178, %struct.ScmObj** %stackaddr$prim47256, align 8
%args46294$cc40176$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47257 = alloca %struct.ScmObj*, align 8
%args46294$cc40176$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40176, %struct.ScmObj* %args46294$cc40176$0)
store volatile %struct.ScmObj* %args46294$cc40176$1, %struct.ScmObj** %stackaddr$prim47257, align 8
%stackaddr$prim47258 = alloca %struct.ScmObj*, align 8
%args46294$cc40176$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40397, %struct.ScmObj* %args46294$cc40176$1)
store volatile %struct.ScmObj* %args46294$cc40176$2, %struct.ScmObj** %stackaddr$prim47258, align 8
%clofunc47259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40176)
musttail call tailcc void %clofunc47259(%struct.ScmObj* %cc40176, %struct.ScmObj* %args46294$cc40176$2)
ret void
}

define tailcc void @proc_clo$ae43315(%struct.ScmObj* %env$ae43315,%struct.ScmObj* %current_45args46295) {
%stackaddr$env-ref47260 = alloca %struct.ScmObj*, align 8
%lst40175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43315, i64 0)
store %struct.ScmObj* %lst40175, %struct.ScmObj** %stackaddr$env-ref47260
%stackaddr$env-ref47261 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43315, i64 1)
store %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$env-ref47261
%stackaddr$env-ref47262 = alloca %struct.ScmObj*, align 8
%k40397 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43315, i64 2)
store %struct.ScmObj* %k40397, %struct.ScmObj** %stackaddr$env-ref47262
%stackaddr$prim47263 = alloca %struct.ScmObj*, align 8
%_95k40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46295)
store volatile %struct.ScmObj* %_95k40399, %struct.ScmObj** %stackaddr$prim47263, align 8
%stackaddr$prim47264 = alloca %struct.ScmObj*, align 8
%current_45args46296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46295)
store volatile %struct.ScmObj* %current_45args46296, %struct.ScmObj** %stackaddr$prim47264, align 8
%stackaddr$prim47265 = alloca %struct.ScmObj*, align 8
%cc40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46296)
store volatile %struct.ScmObj* %cc40176, %struct.ScmObj** %stackaddr$prim47265, align 8
%ae43317 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47266 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43317)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim47266, align 8
%stackaddr$prim47267 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim47267, align 8
%truthy$cmp47268 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40320)
%cmp$cmp47268 = icmp eq i64 %truthy$cmp47268, 1
br i1 %cmp$cmp47268, label %truebranch$cmp47268, label %falsebranch$cmp47268
truebranch$cmp47268:
%ae43321 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43322 = call %struct.ScmObj* @const_init_false()
%args46298$k40397$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47269 = alloca %struct.ScmObj*, align 8
%args46298$k40397$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43322, %struct.ScmObj* %args46298$k40397$0)
store volatile %struct.ScmObj* %args46298$k40397$1, %struct.ScmObj** %stackaddr$prim47269, align 8
%stackaddr$prim47270 = alloca %struct.ScmObj*, align 8
%args46298$k40397$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43321, %struct.ScmObj* %args46298$k40397$1)
store volatile %struct.ScmObj* %args46298$k40397$2, %struct.ScmObj** %stackaddr$prim47270, align 8
%clofunc47271 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40397)
musttail call tailcc void %clofunc47271(%struct.ScmObj* %k40397, %struct.ScmObj* %args46298$k40397$2)
ret void
falsebranch$cmp47268:
%ae43330 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47272 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43330)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim47272, align 8
%stackaddr$prim47273 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim47273, align 8
%stackaddr$prim47274 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40322, %struct.ScmObj* %v40174)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim47274, align 8
%truthy$cmp47275 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40323)
%cmp$cmp47275 = icmp eq i64 %truthy$cmp47275, 1
br i1 %cmp$cmp47275, label %truebranch$cmp47275, label %falsebranch$cmp47275
truebranch$cmp47275:
%ae43336 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47276 = alloca %struct.ScmObj*, align 8
%cpsprim40400 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43336)
store volatile %struct.ScmObj* %cpsprim40400, %struct.ScmObj** %stackaddr$prim47276, align 8
%ae43338 = call %struct.ScmObj* @const_init_int(i64 0)
%args46299$k40397$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47277 = alloca %struct.ScmObj*, align 8
%args46299$k40397$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40400, %struct.ScmObj* %args46299$k40397$0)
store volatile %struct.ScmObj* %args46299$k40397$1, %struct.ScmObj** %stackaddr$prim47277, align 8
%stackaddr$prim47278 = alloca %struct.ScmObj*, align 8
%args46299$k40397$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43338, %struct.ScmObj* %args46299$k40397$1)
store volatile %struct.ScmObj* %args46299$k40397$2, %struct.ScmObj** %stackaddr$prim47278, align 8
%clofunc47279 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40397)
musttail call tailcc void %clofunc47279(%struct.ScmObj* %k40397, %struct.ScmObj* %args46299$k40397$2)
ret void
falsebranch$cmp47275:
%ae43349 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47280 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43349)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim47280, align 8
%stackaddr$prim47281 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim47281, align 8
%ae43352 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47282 = alloca %struct.ScmObj*, align 8
%_95040178 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43352, %struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %_95040178, %struct.ScmObj** %stackaddr$prim47282, align 8
%args46300$cc40176$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47283 = alloca %struct.ScmObj*, align 8
%args46300$cc40176$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40176, %struct.ScmObj* %args46300$cc40176$0)
store volatile %struct.ScmObj* %args46300$cc40176$1, %struct.ScmObj** %stackaddr$prim47283, align 8
%stackaddr$prim47284 = alloca %struct.ScmObj*, align 8
%args46300$cc40176$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40397, %struct.ScmObj* %args46300$cc40176$1)
store volatile %struct.ScmObj* %args46300$cc40176$2, %struct.ScmObj** %stackaddr$prim47284, align 8
%clofunc47285 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40176)
musttail call tailcc void %clofunc47285(%struct.ScmObj* %cc40176, %struct.ScmObj* %args46300$cc40176$2)
ret void
}

define tailcc void @proc_clo$ae43300(%struct.ScmObj* %env$ae43300,%struct.ScmObj* %current_45args46302) {
%stackaddr$prim47286 = alloca %struct.ScmObj*, align 8
%k40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46302)
store volatile %struct.ScmObj* %k40401, %struct.ScmObj** %stackaddr$prim47286, align 8
%stackaddr$prim47287 = alloca %struct.ScmObj*, align 8
%current_45args46303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46302)
store volatile %struct.ScmObj* %current_45args46303, %struct.ScmObj** %stackaddr$prim47287, align 8
%stackaddr$prim47288 = alloca %struct.ScmObj*, align 8
%u40177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46303)
store volatile %struct.ScmObj* %u40177, %struct.ScmObj** %stackaddr$prim47288, align 8
%args46305$u40177$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47289 = alloca %struct.ScmObj*, align 8
%args46305$u40177$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40177, %struct.ScmObj* %args46305$u40177$0)
store volatile %struct.ScmObj* %args46305$u40177$1, %struct.ScmObj** %stackaddr$prim47289, align 8
%stackaddr$prim47290 = alloca %struct.ScmObj*, align 8
%args46305$u40177$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40401, %struct.ScmObj* %args46305$u40177$1)
store volatile %struct.ScmObj* %args46305$u40177$2, %struct.ScmObj** %stackaddr$prim47290, align 8
%clofunc47291 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40177)
musttail call tailcc void %clofunc47291(%struct.ScmObj* %u40177, %struct.ScmObj* %args46305$u40177$2)
ret void
}

define tailcc void @proc_clo$ae42759(%struct.ScmObj* %env$ae42759,%struct.ScmObj* %current_45args46308) {
%stackaddr$prim47292 = alloca %struct.ScmObj*, align 8
%k40402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46308)
store volatile %struct.ScmObj* %k40402, %struct.ScmObj** %stackaddr$prim47292, align 8
%stackaddr$prim47293 = alloca %struct.ScmObj*, align 8
%current_45args46309 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46308)
store volatile %struct.ScmObj* %current_45args46309, %struct.ScmObj** %stackaddr$prim47293, align 8
%stackaddr$prim47294 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46309)
store volatile %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$prim47294, align 8
%stackaddr$prim47295 = alloca %struct.ScmObj*, align 8
%current_45args46310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46309)
store volatile %struct.ScmObj* %current_45args46310, %struct.ScmObj** %stackaddr$prim47295, align 8
%stackaddr$prim47296 = alloca %struct.ScmObj*, align 8
%n40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46310)
store volatile %struct.ScmObj* %n40180, %struct.ScmObj** %stackaddr$prim47296, align 8
%ae42760 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47297 = alloca %struct.ScmObj*, align 8
%n40183 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42760, %struct.ScmObj* %n40180)
store volatile %struct.ScmObj* %n40183, %struct.ScmObj** %stackaddr$prim47297, align 8
%ae42762 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47298 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42762, %struct.ScmObj* %lst40181)
store volatile %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$prim47298, align 8
%stackaddr$makeclosure47299 = alloca %struct.ScmObj*, align 8
%fptrToInt47300 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42764 to i64
%ae42764 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47300)
store volatile %struct.ScmObj* %ae42764, %struct.ScmObj** %stackaddr$makeclosure47299, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42764, %struct.ScmObj* %n40183, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42764, %struct.ScmObj* %lst40182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42764, %struct.ScmObj* %k40402, i64 2)
%ae42765 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47301 = alloca %struct.ScmObj*, align 8
%fptrToInt47302 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42766 to i64
%ae42766 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47302)
store volatile %struct.ScmObj* %ae42766, %struct.ScmObj** %stackaddr$makeclosure47301, align 8
%args46330$ae42764$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47303 = alloca %struct.ScmObj*, align 8
%args46330$ae42764$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42766, %struct.ScmObj* %args46330$ae42764$0)
store volatile %struct.ScmObj* %args46330$ae42764$1, %struct.ScmObj** %stackaddr$prim47303, align 8
%stackaddr$prim47304 = alloca %struct.ScmObj*, align 8
%args46330$ae42764$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42765, %struct.ScmObj* %args46330$ae42764$1)
store volatile %struct.ScmObj* %args46330$ae42764$2, %struct.ScmObj** %stackaddr$prim47304, align 8
%clofunc47305 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42764)
musttail call tailcc void %clofunc47305(%struct.ScmObj* %ae42764, %struct.ScmObj* %args46330$ae42764$2)
ret void
}

define tailcc void @proc_clo$ae42764(%struct.ScmObj* %env$ae42764,%struct.ScmObj* %current_45args46312) {
%stackaddr$env-ref47306 = alloca %struct.ScmObj*, align 8
%n40183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42764, i64 0)
store %struct.ScmObj* %n40183, %struct.ScmObj** %stackaddr$env-ref47306
%stackaddr$env-ref47307 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42764, i64 1)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref47307
%stackaddr$env-ref47308 = alloca %struct.ScmObj*, align 8
%k40402 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42764, i64 2)
store %struct.ScmObj* %k40402, %struct.ScmObj** %stackaddr$env-ref47308
%stackaddr$prim47309 = alloca %struct.ScmObj*, align 8
%_95k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46312)
store volatile %struct.ScmObj* %_95k40403, %struct.ScmObj** %stackaddr$prim47309, align 8
%stackaddr$prim47310 = alloca %struct.ScmObj*, align 8
%current_45args46313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46312)
store volatile %struct.ScmObj* %current_45args46313, %struct.ScmObj** %stackaddr$prim47310, align 8
%stackaddr$prim47311 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46313)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim47311, align 8
%stackaddr$makeclosure47312 = alloca %struct.ScmObj*, align 8
%fptrToInt47313 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42780 to i64
%ae42780 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47313)
store volatile %struct.ScmObj* %ae42780, %struct.ScmObj** %stackaddr$makeclosure47312, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42780, %struct.ScmObj* %n40183, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42780, %struct.ScmObj* %lst40182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42780, %struct.ScmObj* %k40402, i64 2)
%stackaddr$makeclosure47314 = alloca %struct.ScmObj*, align 8
%fptrToInt47315 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42781 to i64
%ae42781 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47315)
store volatile %struct.ScmObj* %ae42781, %struct.ScmObj** %stackaddr$makeclosure47314, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42781, %struct.ScmObj* %n40183, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42781, %struct.ScmObj* %lst40182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42781, %struct.ScmObj* %k40402, i64 2)
%args46325$anf_45bind40311$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47316 = alloca %struct.ScmObj*, align 8
%args46325$anf_45bind40311$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42781, %struct.ScmObj* %args46325$anf_45bind40311$0)
store volatile %struct.ScmObj* %args46325$anf_45bind40311$1, %struct.ScmObj** %stackaddr$prim47316, align 8
%stackaddr$prim47317 = alloca %struct.ScmObj*, align 8
%args46325$anf_45bind40311$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42780, %struct.ScmObj* %args46325$anf_45bind40311$1)
store volatile %struct.ScmObj* %args46325$anf_45bind40311$2, %struct.ScmObj** %stackaddr$prim47317, align 8
%clofunc47318 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40311)
musttail call tailcc void %clofunc47318(%struct.ScmObj* %anf_45bind40311, %struct.ScmObj* %args46325$anf_45bind40311$2)
ret void
}

define tailcc void @proc_clo$ae42780(%struct.ScmObj* %env$ae42780,%struct.ScmObj* %current_45args46315) {
%stackaddr$env-ref47319 = alloca %struct.ScmObj*, align 8
%n40183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42780, i64 0)
store %struct.ScmObj* %n40183, %struct.ScmObj** %stackaddr$env-ref47319
%stackaddr$env-ref47320 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42780, i64 1)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref47320
%stackaddr$env-ref47321 = alloca %struct.ScmObj*, align 8
%k40402 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42780, i64 2)
store %struct.ScmObj* %k40402, %struct.ScmObj** %stackaddr$env-ref47321
%stackaddr$prim47322 = alloca %struct.ScmObj*, align 8
%_95k40404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46315)
store volatile %struct.ScmObj* %_95k40404, %struct.ScmObj** %stackaddr$prim47322, align 8
%stackaddr$prim47323 = alloca %struct.ScmObj*, align 8
%current_45args46316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46315)
store volatile %struct.ScmObj* %current_45args46316, %struct.ScmObj** %stackaddr$prim47323, align 8
%stackaddr$prim47324 = alloca %struct.ScmObj*, align 8
%cc40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46316)
store volatile %struct.ScmObj* %cc40184, %struct.ScmObj** %stackaddr$prim47324, align 8
%ae42923 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47325 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42923)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim47325, align 8
%ae42924 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47326 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42924, %struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim47326, align 8
%truthy$cmp47327 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40313)
%cmp$cmp47327 = icmp eq i64 %truthy$cmp47327, 1
br i1 %cmp$cmp47327, label %truebranch$cmp47327, label %falsebranch$cmp47327
truebranch$cmp47327:
%ae42928 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47328 = alloca %struct.ScmObj*, align 8
%cpsprim40405 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42928)
store volatile %struct.ScmObj* %cpsprim40405, %struct.ScmObj** %stackaddr$prim47328, align 8
%ae42930 = call %struct.ScmObj* @const_init_int(i64 0)
%args46318$k40402$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47329 = alloca %struct.ScmObj*, align 8
%args46318$k40402$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40405, %struct.ScmObj* %args46318$k40402$0)
store volatile %struct.ScmObj* %args46318$k40402$1, %struct.ScmObj** %stackaddr$prim47329, align 8
%stackaddr$prim47330 = alloca %struct.ScmObj*, align 8
%args46318$k40402$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42930, %struct.ScmObj* %args46318$k40402$1)
store volatile %struct.ScmObj* %args46318$k40402$2, %struct.ScmObj** %stackaddr$prim47330, align 8
%clofunc47331 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40402)
musttail call tailcc void %clofunc47331(%struct.ScmObj* %k40402, %struct.ScmObj* %args46318$k40402$2)
ret void
falsebranch$cmp47327:
%ae42941 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47332 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42941)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim47332, align 8
%stackaddr$prim47333 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim47333, align 8
%ae42944 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47334 = alloca %struct.ScmObj*, align 8
%_95040187 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42944, %struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %_95040187, %struct.ScmObj** %stackaddr$prim47334, align 8
%ae42947 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47335 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42947)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim47335, align 8
%ae42949 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47336 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40316, %struct.ScmObj* %ae42949)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim47336, align 8
%ae42951 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47337 = alloca %struct.ScmObj*, align 8
%_95140186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42951, %struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %_95140186, %struct.ScmObj** %stackaddr$prim47337, align 8
%args46319$cc40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47338 = alloca %struct.ScmObj*, align 8
%args46319$cc40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40184, %struct.ScmObj* %args46319$cc40184$0)
store volatile %struct.ScmObj* %args46319$cc40184$1, %struct.ScmObj** %stackaddr$prim47338, align 8
%stackaddr$prim47339 = alloca %struct.ScmObj*, align 8
%args46319$cc40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40402, %struct.ScmObj* %args46319$cc40184$1)
store volatile %struct.ScmObj* %args46319$cc40184$2, %struct.ScmObj** %stackaddr$prim47339, align 8
%clofunc47340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40184)
musttail call tailcc void %clofunc47340(%struct.ScmObj* %cc40184, %struct.ScmObj* %args46319$cc40184$2)
ret void
}

define tailcc void @proc_clo$ae42781(%struct.ScmObj* %env$ae42781,%struct.ScmObj* %current_45args46320) {
%stackaddr$env-ref47341 = alloca %struct.ScmObj*, align 8
%n40183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42781, i64 0)
store %struct.ScmObj* %n40183, %struct.ScmObj** %stackaddr$env-ref47341
%stackaddr$env-ref47342 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42781, i64 1)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref47342
%stackaddr$env-ref47343 = alloca %struct.ScmObj*, align 8
%k40402 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42781, i64 2)
store %struct.ScmObj* %k40402, %struct.ScmObj** %stackaddr$env-ref47343
%stackaddr$prim47344 = alloca %struct.ScmObj*, align 8
%_95k40404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46320)
store volatile %struct.ScmObj* %_95k40404, %struct.ScmObj** %stackaddr$prim47344, align 8
%stackaddr$prim47345 = alloca %struct.ScmObj*, align 8
%current_45args46321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46320)
store volatile %struct.ScmObj* %current_45args46321, %struct.ScmObj** %stackaddr$prim47345, align 8
%stackaddr$prim47346 = alloca %struct.ScmObj*, align 8
%cc40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46321)
store volatile %struct.ScmObj* %cc40184, %struct.ScmObj** %stackaddr$prim47346, align 8
%ae42783 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47347 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42783)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim47347, align 8
%ae42784 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47348 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42784, %struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim47348, align 8
%truthy$cmp47349 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40313)
%cmp$cmp47349 = icmp eq i64 %truthy$cmp47349, 1
br i1 %cmp$cmp47349, label %truebranch$cmp47349, label %falsebranch$cmp47349
truebranch$cmp47349:
%ae42788 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47350 = alloca %struct.ScmObj*, align 8
%cpsprim40405 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42788)
store volatile %struct.ScmObj* %cpsprim40405, %struct.ScmObj** %stackaddr$prim47350, align 8
%ae42790 = call %struct.ScmObj* @const_init_int(i64 0)
%args46323$k40402$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47351 = alloca %struct.ScmObj*, align 8
%args46323$k40402$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40405, %struct.ScmObj* %args46323$k40402$0)
store volatile %struct.ScmObj* %args46323$k40402$1, %struct.ScmObj** %stackaddr$prim47351, align 8
%stackaddr$prim47352 = alloca %struct.ScmObj*, align 8
%args46323$k40402$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42790, %struct.ScmObj* %args46323$k40402$1)
store volatile %struct.ScmObj* %args46323$k40402$2, %struct.ScmObj** %stackaddr$prim47352, align 8
%clofunc47353 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40402)
musttail call tailcc void %clofunc47353(%struct.ScmObj* %k40402, %struct.ScmObj* %args46323$k40402$2)
ret void
falsebranch$cmp47349:
%ae42801 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47354 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42801)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim47354, align 8
%stackaddr$prim47355 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim47355, align 8
%ae42804 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47356 = alloca %struct.ScmObj*, align 8
%_95040187 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42804, %struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %_95040187, %struct.ScmObj** %stackaddr$prim47356, align 8
%ae42807 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47357 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42807)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim47357, align 8
%ae42809 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47358 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40316, %struct.ScmObj* %ae42809)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim47358, align 8
%ae42811 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47359 = alloca %struct.ScmObj*, align 8
%_95140186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42811, %struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %_95140186, %struct.ScmObj** %stackaddr$prim47359, align 8
%args46324$cc40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47360 = alloca %struct.ScmObj*, align 8
%args46324$cc40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40184, %struct.ScmObj* %args46324$cc40184$0)
store volatile %struct.ScmObj* %args46324$cc40184$1, %struct.ScmObj** %stackaddr$prim47360, align 8
%stackaddr$prim47361 = alloca %struct.ScmObj*, align 8
%args46324$cc40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40402, %struct.ScmObj* %args46324$cc40184$1)
store volatile %struct.ScmObj* %args46324$cc40184$2, %struct.ScmObj** %stackaddr$prim47361, align 8
%clofunc47362 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40184)
musttail call tailcc void %clofunc47362(%struct.ScmObj* %cc40184, %struct.ScmObj* %args46324$cc40184$2)
ret void
}

define tailcc void @proc_clo$ae42766(%struct.ScmObj* %env$ae42766,%struct.ScmObj* %current_45args46326) {
%stackaddr$prim47363 = alloca %struct.ScmObj*, align 8
%k40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46326)
store volatile %struct.ScmObj* %k40406, %struct.ScmObj** %stackaddr$prim47363, align 8
%stackaddr$prim47364 = alloca %struct.ScmObj*, align 8
%current_45args46327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46326)
store volatile %struct.ScmObj* %current_45args46327, %struct.ScmObj** %stackaddr$prim47364, align 8
%stackaddr$prim47365 = alloca %struct.ScmObj*, align 8
%u40185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46327)
store volatile %struct.ScmObj* %u40185, %struct.ScmObj** %stackaddr$prim47365, align 8
%args46329$u40185$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47366 = alloca %struct.ScmObj*, align 8
%args46329$u40185$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40185, %struct.ScmObj* %args46329$u40185$0)
store volatile %struct.ScmObj* %args46329$u40185$1, %struct.ScmObj** %stackaddr$prim47366, align 8
%stackaddr$prim47367 = alloca %struct.ScmObj*, align 8
%args46329$u40185$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40406, %struct.ScmObj* %args46329$u40185$1)
store volatile %struct.ScmObj* %args46329$u40185$2, %struct.ScmObj** %stackaddr$prim47367, align 8
%clofunc47368 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40185)
musttail call tailcc void %clofunc47368(%struct.ScmObj* %u40185, %struct.ScmObj* %args46329$u40185$2)
ret void
}

define tailcc void @proc_clo$ae42343(%struct.ScmObj* %env$ae42343,%struct.ScmObj* %current_45args46332) {
%stackaddr$prim47369 = alloca %struct.ScmObj*, align 8
%k40407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46332)
store volatile %struct.ScmObj* %k40407, %struct.ScmObj** %stackaddr$prim47369, align 8
%stackaddr$prim47370 = alloca %struct.ScmObj*, align 8
%current_45args46333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46332)
store volatile %struct.ScmObj* %current_45args46333, %struct.ScmObj** %stackaddr$prim47370, align 8
%stackaddr$prim47371 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46333)
store volatile %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$prim47371, align 8
%ae42344 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47372 = alloca %struct.ScmObj*, align 8
%a40190 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42344, %struct.ScmObj* %a40189)
store volatile %struct.ScmObj* %a40190, %struct.ScmObj** %stackaddr$prim47372, align 8
%stackaddr$makeclosure47373 = alloca %struct.ScmObj*, align 8
%fptrToInt47374 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42346 to i64
%ae42346 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47374)
store volatile %struct.ScmObj* %ae42346, %struct.ScmObj** %stackaddr$makeclosure47373, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42346, %struct.ScmObj* %k40407, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42346, %struct.ScmObj* %a40190, i64 1)
%ae42347 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47375 = alloca %struct.ScmObj*, align 8
%fptrToInt47376 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42348 to i64
%ae42348 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47376)
store volatile %struct.ScmObj* %ae42348, %struct.ScmObj** %stackaddr$makeclosure47375, align 8
%args46355$ae42346$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47377 = alloca %struct.ScmObj*, align 8
%args46355$ae42346$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42348, %struct.ScmObj* %args46355$ae42346$0)
store volatile %struct.ScmObj* %args46355$ae42346$1, %struct.ScmObj** %stackaddr$prim47377, align 8
%stackaddr$prim47378 = alloca %struct.ScmObj*, align 8
%args46355$ae42346$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42347, %struct.ScmObj* %args46355$ae42346$1)
store volatile %struct.ScmObj* %args46355$ae42346$2, %struct.ScmObj** %stackaddr$prim47378, align 8
%clofunc47379 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42346)
musttail call tailcc void %clofunc47379(%struct.ScmObj* %ae42346, %struct.ScmObj* %args46355$ae42346$2)
ret void
}

define tailcc void @proc_clo$ae42346(%struct.ScmObj* %env$ae42346,%struct.ScmObj* %current_45args46335) {
%stackaddr$env-ref47380 = alloca %struct.ScmObj*, align 8
%k40407 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42346, i64 0)
store %struct.ScmObj* %k40407, %struct.ScmObj** %stackaddr$env-ref47380
%stackaddr$env-ref47381 = alloca %struct.ScmObj*, align 8
%a40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42346, i64 1)
store %struct.ScmObj* %a40190, %struct.ScmObj** %stackaddr$env-ref47381
%stackaddr$prim47382 = alloca %struct.ScmObj*, align 8
%_95k40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46335)
store volatile %struct.ScmObj* %_95k40408, %struct.ScmObj** %stackaddr$prim47382, align 8
%stackaddr$prim47383 = alloca %struct.ScmObj*, align 8
%current_45args46336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46335)
store volatile %struct.ScmObj* %current_45args46336, %struct.ScmObj** %stackaddr$prim47383, align 8
%stackaddr$prim47384 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46336)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim47384, align 8
%stackaddr$makeclosure47385 = alloca %struct.ScmObj*, align 8
%fptrToInt47386 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42365 to i64
%ae42365 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47386)
store volatile %struct.ScmObj* %ae42365, %struct.ScmObj** %stackaddr$makeclosure47385, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42365, %struct.ScmObj* %k40407, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42365, %struct.ScmObj* %a40190, i64 1)
%stackaddr$makeclosure47387 = alloca %struct.ScmObj*, align 8
%fptrToInt47388 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42366 to i64
%ae42366 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47388)
store volatile %struct.ScmObj* %ae42366, %struct.ScmObj** %stackaddr$makeclosure47387, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42366, %struct.ScmObj* %k40407, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42366, %struct.ScmObj* %a40190, i64 1)
%args46350$anf_45bind40303$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47389 = alloca %struct.ScmObj*, align 8
%args46350$anf_45bind40303$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42366, %struct.ScmObj* %args46350$anf_45bind40303$0)
store volatile %struct.ScmObj* %args46350$anf_45bind40303$1, %struct.ScmObj** %stackaddr$prim47389, align 8
%stackaddr$prim47390 = alloca %struct.ScmObj*, align 8
%args46350$anf_45bind40303$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42365, %struct.ScmObj* %args46350$anf_45bind40303$1)
store volatile %struct.ScmObj* %args46350$anf_45bind40303$2, %struct.ScmObj** %stackaddr$prim47390, align 8
%clofunc47391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40303)
musttail call tailcc void %clofunc47391(%struct.ScmObj* %anf_45bind40303, %struct.ScmObj* %args46350$anf_45bind40303$2)
ret void
}

define tailcc void @proc_clo$ae42365(%struct.ScmObj* %env$ae42365,%struct.ScmObj* %current_45args46338) {
%stackaddr$env-ref47392 = alloca %struct.ScmObj*, align 8
%k40407 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42365, i64 0)
store %struct.ScmObj* %k40407, %struct.ScmObj** %stackaddr$env-ref47392
%stackaddr$env-ref47393 = alloca %struct.ScmObj*, align 8
%a40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42365, i64 1)
store %struct.ScmObj* %a40190, %struct.ScmObj** %stackaddr$env-ref47393
%stackaddr$prim47394 = alloca %struct.ScmObj*, align 8
%_95k40409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46338)
store volatile %struct.ScmObj* %_95k40409, %struct.ScmObj** %stackaddr$prim47394, align 8
%stackaddr$prim47395 = alloca %struct.ScmObj*, align 8
%current_45args46339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46338)
store volatile %struct.ScmObj* %current_45args46339, %struct.ScmObj** %stackaddr$prim47395, align 8
%stackaddr$prim47396 = alloca %struct.ScmObj*, align 8
%cc40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46339)
store volatile %struct.ScmObj* %cc40191, %struct.ScmObj** %stackaddr$prim47396, align 8
%ae42481 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47397 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42481)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim47397, align 8
%stackaddr$prim47398 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40304)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim47398, align 8
%truthy$cmp47399 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40305)
%cmp$cmp47399 = icmp eq i64 %truthy$cmp47399, 1
br i1 %cmp$cmp47399, label %truebranch$cmp47399, label %falsebranch$cmp47399
truebranch$cmp47399:
%ae42485 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42486 = call %struct.ScmObj* @const_init_true()
%args46341$k40407$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47400 = alloca %struct.ScmObj*, align 8
%args46341$k40407$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42486, %struct.ScmObj* %args46341$k40407$0)
store volatile %struct.ScmObj* %args46341$k40407$1, %struct.ScmObj** %stackaddr$prim47400, align 8
%stackaddr$prim47401 = alloca %struct.ScmObj*, align 8
%args46341$k40407$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42485, %struct.ScmObj* %args46341$k40407$1)
store volatile %struct.ScmObj* %args46341$k40407$2, %struct.ScmObj** %stackaddr$prim47401, align 8
%clofunc47402 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40407)
musttail call tailcc void %clofunc47402(%struct.ScmObj* %k40407, %struct.ScmObj* %args46341$k40407$2)
ret void
falsebranch$cmp47399:
%ae42494 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47403 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42494)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim47403, align 8
%stackaddr$prim47404 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim47404, align 8
%truthy$cmp47405 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40307)
%cmp$cmp47405 = icmp eq i64 %truthy$cmp47405, 1
br i1 %cmp$cmp47405, label %truebranch$cmp47405, label %falsebranch$cmp47405
truebranch$cmp47405:
%ae42498 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47406 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42498)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim47406, align 8
%stackaddr$prim47407 = alloca %struct.ScmObj*, align 8
%b40193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40308)
store volatile %struct.ScmObj* %b40193, %struct.ScmObj** %stackaddr$prim47407, align 8
%ae42501 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47408 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42501)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim47408, align 8
%stackaddr$prim47409 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim47409, align 8
%ae42504 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47410 = alloca %struct.ScmObj*, align 8
%_95040194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42504, %struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %_95040194, %struct.ScmObj** %stackaddr$prim47410, align 8
%args46342$cc40191$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47411 = alloca %struct.ScmObj*, align 8
%args46342$cc40191$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40191, %struct.ScmObj* %args46342$cc40191$0)
store volatile %struct.ScmObj* %args46342$cc40191$1, %struct.ScmObj** %stackaddr$prim47411, align 8
%stackaddr$prim47412 = alloca %struct.ScmObj*, align 8
%args46342$cc40191$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40407, %struct.ScmObj* %args46342$cc40191$1)
store volatile %struct.ScmObj* %args46342$cc40191$2, %struct.ScmObj** %stackaddr$prim47412, align 8
%clofunc47413 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40191)
musttail call tailcc void %clofunc47413(%struct.ScmObj* %cc40191, %struct.ScmObj* %args46342$cc40191$2)
ret void
falsebranch$cmp47405:
%ae42537 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42538 = call %struct.ScmObj* @const_init_false()
%args46343$k40407$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47414 = alloca %struct.ScmObj*, align 8
%args46343$k40407$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42538, %struct.ScmObj* %args46343$k40407$0)
store volatile %struct.ScmObj* %args46343$k40407$1, %struct.ScmObj** %stackaddr$prim47414, align 8
%stackaddr$prim47415 = alloca %struct.ScmObj*, align 8
%args46343$k40407$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42537, %struct.ScmObj* %args46343$k40407$1)
store volatile %struct.ScmObj* %args46343$k40407$2, %struct.ScmObj** %stackaddr$prim47415, align 8
%clofunc47416 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40407)
musttail call tailcc void %clofunc47416(%struct.ScmObj* %k40407, %struct.ScmObj* %args46343$k40407$2)
ret void
}

define tailcc void @proc_clo$ae42366(%struct.ScmObj* %env$ae42366,%struct.ScmObj* %current_45args46344) {
%stackaddr$env-ref47417 = alloca %struct.ScmObj*, align 8
%k40407 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42366, i64 0)
store %struct.ScmObj* %k40407, %struct.ScmObj** %stackaddr$env-ref47417
%stackaddr$env-ref47418 = alloca %struct.ScmObj*, align 8
%a40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42366, i64 1)
store %struct.ScmObj* %a40190, %struct.ScmObj** %stackaddr$env-ref47418
%stackaddr$prim47419 = alloca %struct.ScmObj*, align 8
%_95k40409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46344)
store volatile %struct.ScmObj* %_95k40409, %struct.ScmObj** %stackaddr$prim47419, align 8
%stackaddr$prim47420 = alloca %struct.ScmObj*, align 8
%current_45args46345 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46344)
store volatile %struct.ScmObj* %current_45args46345, %struct.ScmObj** %stackaddr$prim47420, align 8
%stackaddr$prim47421 = alloca %struct.ScmObj*, align 8
%cc40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46345)
store volatile %struct.ScmObj* %cc40191, %struct.ScmObj** %stackaddr$prim47421, align 8
%ae42368 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47422 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42368)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim47422, align 8
%stackaddr$prim47423 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40304)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim47423, align 8
%truthy$cmp47424 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40305)
%cmp$cmp47424 = icmp eq i64 %truthy$cmp47424, 1
br i1 %cmp$cmp47424, label %truebranch$cmp47424, label %falsebranch$cmp47424
truebranch$cmp47424:
%ae42372 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42373 = call %struct.ScmObj* @const_init_true()
%args46347$k40407$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47425 = alloca %struct.ScmObj*, align 8
%args46347$k40407$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42373, %struct.ScmObj* %args46347$k40407$0)
store volatile %struct.ScmObj* %args46347$k40407$1, %struct.ScmObj** %stackaddr$prim47425, align 8
%stackaddr$prim47426 = alloca %struct.ScmObj*, align 8
%args46347$k40407$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42372, %struct.ScmObj* %args46347$k40407$1)
store volatile %struct.ScmObj* %args46347$k40407$2, %struct.ScmObj** %stackaddr$prim47426, align 8
%clofunc47427 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40407)
musttail call tailcc void %clofunc47427(%struct.ScmObj* %k40407, %struct.ScmObj* %args46347$k40407$2)
ret void
falsebranch$cmp47424:
%ae42381 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47428 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42381)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim47428, align 8
%stackaddr$prim47429 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim47429, align 8
%truthy$cmp47430 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40307)
%cmp$cmp47430 = icmp eq i64 %truthy$cmp47430, 1
br i1 %cmp$cmp47430, label %truebranch$cmp47430, label %falsebranch$cmp47430
truebranch$cmp47430:
%ae42385 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47431 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42385)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim47431, align 8
%stackaddr$prim47432 = alloca %struct.ScmObj*, align 8
%b40193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40308)
store volatile %struct.ScmObj* %b40193, %struct.ScmObj** %stackaddr$prim47432, align 8
%ae42388 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47433 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42388)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim47433, align 8
%stackaddr$prim47434 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim47434, align 8
%ae42391 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47435 = alloca %struct.ScmObj*, align 8
%_95040194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42391, %struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %_95040194, %struct.ScmObj** %stackaddr$prim47435, align 8
%args46348$cc40191$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47436 = alloca %struct.ScmObj*, align 8
%args46348$cc40191$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40191, %struct.ScmObj* %args46348$cc40191$0)
store volatile %struct.ScmObj* %args46348$cc40191$1, %struct.ScmObj** %stackaddr$prim47436, align 8
%stackaddr$prim47437 = alloca %struct.ScmObj*, align 8
%args46348$cc40191$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40407, %struct.ScmObj* %args46348$cc40191$1)
store volatile %struct.ScmObj* %args46348$cc40191$2, %struct.ScmObj** %stackaddr$prim47437, align 8
%clofunc47438 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40191)
musttail call tailcc void %clofunc47438(%struct.ScmObj* %cc40191, %struct.ScmObj* %args46348$cc40191$2)
ret void
falsebranch$cmp47430:
%ae42424 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42425 = call %struct.ScmObj* @const_init_false()
%args46349$k40407$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47439 = alloca %struct.ScmObj*, align 8
%args46349$k40407$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42425, %struct.ScmObj* %args46349$k40407$0)
store volatile %struct.ScmObj* %args46349$k40407$1, %struct.ScmObj** %stackaddr$prim47439, align 8
%stackaddr$prim47440 = alloca %struct.ScmObj*, align 8
%args46349$k40407$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42424, %struct.ScmObj* %args46349$k40407$1)
store volatile %struct.ScmObj* %args46349$k40407$2, %struct.ScmObj** %stackaddr$prim47440, align 8
%clofunc47441 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40407)
musttail call tailcc void %clofunc47441(%struct.ScmObj* %k40407, %struct.ScmObj* %args46349$k40407$2)
ret void
}

define tailcc void @proc_clo$ae42348(%struct.ScmObj* %env$ae42348,%struct.ScmObj* %current_45args46351) {
%stackaddr$prim47442 = alloca %struct.ScmObj*, align 8
%k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46351)
store volatile %struct.ScmObj* %k40410, %struct.ScmObj** %stackaddr$prim47442, align 8
%stackaddr$prim47443 = alloca %struct.ScmObj*, align 8
%current_45args46352 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46351)
store volatile %struct.ScmObj* %current_45args46352, %struct.ScmObj** %stackaddr$prim47443, align 8
%stackaddr$prim47444 = alloca %struct.ScmObj*, align 8
%k40192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46352)
store volatile %struct.ScmObj* %k40192, %struct.ScmObj** %stackaddr$prim47444, align 8
%ae42350 = call %struct.ScmObj* @const_init_int(i64 0)
%args46354$k40410$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47445 = alloca %struct.ScmObj*, align 8
%args46354$k40410$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40192, %struct.ScmObj* %args46354$k40410$0)
store volatile %struct.ScmObj* %args46354$k40410$1, %struct.ScmObj** %stackaddr$prim47445, align 8
%stackaddr$prim47446 = alloca %struct.ScmObj*, align 8
%args46354$k40410$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42350, %struct.ScmObj* %args46354$k40410$1)
store volatile %struct.ScmObj* %args46354$k40410$2, %struct.ScmObj** %stackaddr$prim47446, align 8
%clofunc47447 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40410)
musttail call tailcc void %clofunc47447(%struct.ScmObj* %k40410, %struct.ScmObj* %args46354$k40410$2)
ret void
}

define tailcc void @proc_clo$ae42271(%struct.ScmObj* %env$ae42271,%struct.ScmObj* %current_45args46357) {
%stackaddr$env-ref47448 = alloca %struct.ScmObj*, align 8
%_37append40196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42271, i64 0)
store %struct.ScmObj* %_37append40196, %struct.ScmObj** %stackaddr$env-ref47448
%stackaddr$prim47449 = alloca %struct.ScmObj*, align 8
%k40411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46357)
store volatile %struct.ScmObj* %k40411, %struct.ScmObj** %stackaddr$prim47449, align 8
%stackaddr$prim47450 = alloca %struct.ScmObj*, align 8
%current_45args46358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46357)
store volatile %struct.ScmObj* %current_45args46358, %struct.ScmObj** %stackaddr$prim47450, align 8
%stackaddr$prim47451 = alloca %struct.ScmObj*, align 8
%ls040199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46358)
store volatile %struct.ScmObj* %ls040199, %struct.ScmObj** %stackaddr$prim47451, align 8
%stackaddr$prim47452 = alloca %struct.ScmObj*, align 8
%current_45args46359 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46358)
store volatile %struct.ScmObj* %current_45args46359, %struct.ScmObj** %stackaddr$prim47452, align 8
%stackaddr$prim47453 = alloca %struct.ScmObj*, align 8
%ls140198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46359)
store volatile %struct.ScmObj* %ls140198, %struct.ScmObj** %stackaddr$prim47453, align 8
%stackaddr$prim47454 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040199)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim47454, align 8
%truthy$cmp47455 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40297)
%cmp$cmp47455 = icmp eq i64 %truthy$cmp47455, 1
br i1 %cmp$cmp47455, label %truebranch$cmp47455, label %falsebranch$cmp47455
truebranch$cmp47455:
%ae42275 = call %struct.ScmObj* @const_init_int(i64 0)
%args46361$k40411$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47456 = alloca %struct.ScmObj*, align 8
%args46361$k40411$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140198, %struct.ScmObj* %args46361$k40411$0)
store volatile %struct.ScmObj* %args46361$k40411$1, %struct.ScmObj** %stackaddr$prim47456, align 8
%stackaddr$prim47457 = alloca %struct.ScmObj*, align 8
%args46361$k40411$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42275, %struct.ScmObj* %args46361$k40411$1)
store volatile %struct.ScmObj* %args46361$k40411$2, %struct.ScmObj** %stackaddr$prim47457, align 8
%clofunc47458 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40411)
musttail call tailcc void %clofunc47458(%struct.ScmObj* %k40411, %struct.ScmObj* %args46361$k40411$2)
ret void
falsebranch$cmp47455:
%stackaddr$prim47459 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040199)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim47459, align 8
%ae42282 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47460 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40196, %struct.ScmObj* %ae42282)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim47460, align 8
%stackaddr$prim47461 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040199)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim47461, align 8
%stackaddr$makeclosure47462 = alloca %struct.ScmObj*, align 8
%fptrToInt47463 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42285 to i64
%ae42285 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47463)
store volatile %struct.ScmObj* %ae42285, %struct.ScmObj** %stackaddr$makeclosure47462, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42285, %struct.ScmObj* %k40411, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42285, %struct.ScmObj* %anf_45bind40298, i64 1)
%args46366$anf_45bind40299$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47464 = alloca %struct.ScmObj*, align 8
%args46366$anf_45bind40299$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140198, %struct.ScmObj* %args46366$anf_45bind40299$0)
store volatile %struct.ScmObj* %args46366$anf_45bind40299$1, %struct.ScmObj** %stackaddr$prim47464, align 8
%stackaddr$prim47465 = alloca %struct.ScmObj*, align 8
%args46366$anf_45bind40299$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40300, %struct.ScmObj* %args46366$anf_45bind40299$1)
store volatile %struct.ScmObj* %args46366$anf_45bind40299$2, %struct.ScmObj** %stackaddr$prim47465, align 8
%stackaddr$prim47466 = alloca %struct.ScmObj*, align 8
%args46366$anf_45bind40299$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42285, %struct.ScmObj* %args46366$anf_45bind40299$2)
store volatile %struct.ScmObj* %args46366$anf_45bind40299$3, %struct.ScmObj** %stackaddr$prim47466, align 8
%clofunc47467 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40299)
musttail call tailcc void %clofunc47467(%struct.ScmObj* %anf_45bind40299, %struct.ScmObj* %args46366$anf_45bind40299$3)
ret void
}

define tailcc void @proc_clo$ae42285(%struct.ScmObj* %env$ae42285,%struct.ScmObj* %current_45args46362) {
%stackaddr$env-ref47468 = alloca %struct.ScmObj*, align 8
%k40411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42285, i64 0)
store %struct.ScmObj* %k40411, %struct.ScmObj** %stackaddr$env-ref47468
%stackaddr$env-ref47469 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42285, i64 1)
store %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$env-ref47469
%stackaddr$prim47470 = alloca %struct.ScmObj*, align 8
%_95k40412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46362)
store volatile %struct.ScmObj* %_95k40412, %struct.ScmObj** %stackaddr$prim47470, align 8
%stackaddr$prim47471 = alloca %struct.ScmObj*, align 8
%current_45args46363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46362)
store volatile %struct.ScmObj* %current_45args46363, %struct.ScmObj** %stackaddr$prim47471, align 8
%stackaddr$prim47472 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46363)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim47472, align 8
%stackaddr$prim47473 = alloca %struct.ScmObj*, align 8
%cpsprim40413 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40298, %struct.ScmObj* %anf_45bind40301)
store volatile %struct.ScmObj* %cpsprim40413, %struct.ScmObj** %stackaddr$prim47473, align 8
%ae42291 = call %struct.ScmObj* @const_init_int(i64 0)
%args46365$k40411$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47474 = alloca %struct.ScmObj*, align 8
%args46365$k40411$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40413, %struct.ScmObj* %args46365$k40411$0)
store volatile %struct.ScmObj* %args46365$k40411$1, %struct.ScmObj** %stackaddr$prim47474, align 8
%stackaddr$prim47475 = alloca %struct.ScmObj*, align 8
%args46365$k40411$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42291, %struct.ScmObj* %args46365$k40411$1)
store volatile %struct.ScmObj* %args46365$k40411$2, %struct.ScmObj** %stackaddr$prim47475, align 8
%clofunc47476 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40411)
musttail call tailcc void %clofunc47476(%struct.ScmObj* %k40411, %struct.ScmObj* %args46365$k40411$2)
ret void
}

define tailcc void @proc_clo$ae42245(%struct.ScmObj* %env$ae42245,%struct.ScmObj* %current_45args46368) {
%stackaddr$prim47477 = alloca %struct.ScmObj*, align 8
%k40414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46368)
store volatile %struct.ScmObj* %k40414, %struct.ScmObj** %stackaddr$prim47477, align 8
%stackaddr$prim47478 = alloca %struct.ScmObj*, align 8
%current_45args46369 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46368)
store volatile %struct.ScmObj* %current_45args46369, %struct.ScmObj** %stackaddr$prim47478, align 8
%stackaddr$prim47479 = alloca %struct.ScmObj*, align 8
%a40202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46369)
store volatile %struct.ScmObj* %a40202, %struct.ScmObj** %stackaddr$prim47479, align 8
%stackaddr$prim47480 = alloca %struct.ScmObj*, align 8
%current_45args46370 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46369)
store volatile %struct.ScmObj* %current_45args46370, %struct.ScmObj** %stackaddr$prim47480, align 8
%stackaddr$prim47481 = alloca %struct.ScmObj*, align 8
%b40201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46370)
store volatile %struct.ScmObj* %b40201, %struct.ScmObj** %stackaddr$prim47481, align 8
%stackaddr$prim47482 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40202, %struct.ScmObj* %b40201)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim47482, align 8
%stackaddr$prim47483 = alloca %struct.ScmObj*, align 8
%cpsprim40415 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40296)
store volatile %struct.ScmObj* %cpsprim40415, %struct.ScmObj** %stackaddr$prim47483, align 8
%ae42250 = call %struct.ScmObj* @const_init_int(i64 0)
%args46372$k40414$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47484 = alloca %struct.ScmObj*, align 8
%args46372$k40414$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40415, %struct.ScmObj* %args46372$k40414$0)
store volatile %struct.ScmObj* %args46372$k40414$1, %struct.ScmObj** %stackaddr$prim47484, align 8
%stackaddr$prim47485 = alloca %struct.ScmObj*, align 8
%args46372$k40414$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42250, %struct.ScmObj* %args46372$k40414$1)
store volatile %struct.ScmObj* %args46372$k40414$2, %struct.ScmObj** %stackaddr$prim47485, align 8
%clofunc47486 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40414)
musttail call tailcc void %clofunc47486(%struct.ScmObj* %k40414, %struct.ScmObj* %args46372$k40414$2)
ret void
}

define tailcc void @proc_clo$ae42221(%struct.ScmObj* %env$ae42221,%struct.ScmObj* %current_45args46374) {
%stackaddr$prim47487 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46374)
store volatile %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$prim47487, align 8
%stackaddr$prim47488 = alloca %struct.ScmObj*, align 8
%current_45args46375 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46374)
store volatile %struct.ScmObj* %current_45args46375, %struct.ScmObj** %stackaddr$prim47488, align 8
%stackaddr$prim47489 = alloca %struct.ScmObj*, align 8
%a40205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46375)
store volatile %struct.ScmObj* %a40205, %struct.ScmObj** %stackaddr$prim47489, align 8
%stackaddr$prim47490 = alloca %struct.ScmObj*, align 8
%current_45args46376 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46375)
store volatile %struct.ScmObj* %current_45args46376, %struct.ScmObj** %stackaddr$prim47490, align 8
%stackaddr$prim47491 = alloca %struct.ScmObj*, align 8
%b40204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46376)
store volatile %struct.ScmObj* %b40204, %struct.ScmObj** %stackaddr$prim47491, align 8
%stackaddr$prim47492 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40205, %struct.ScmObj* %b40204)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim47492, align 8
%stackaddr$prim47493 = alloca %struct.ScmObj*, align 8
%cpsprim40417 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40295)
store volatile %struct.ScmObj* %cpsprim40417, %struct.ScmObj** %stackaddr$prim47493, align 8
%ae42226 = call %struct.ScmObj* @const_init_int(i64 0)
%args46378$k40416$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47494 = alloca %struct.ScmObj*, align 8
%args46378$k40416$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40417, %struct.ScmObj* %args46378$k40416$0)
store volatile %struct.ScmObj* %args46378$k40416$1, %struct.ScmObj** %stackaddr$prim47494, align 8
%stackaddr$prim47495 = alloca %struct.ScmObj*, align 8
%args46378$k40416$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42226, %struct.ScmObj* %args46378$k40416$1)
store volatile %struct.ScmObj* %args46378$k40416$2, %struct.ScmObj** %stackaddr$prim47495, align 8
%clofunc47496 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40416)
musttail call tailcc void %clofunc47496(%struct.ScmObj* %k40416, %struct.ScmObj* %args46378$k40416$2)
ret void
}

define tailcc void @proc_clo$ae41827(%struct.ScmObj* %env$ae41827,%struct.ScmObj* %current_45args46381) {
%stackaddr$env-ref47497 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41827, i64 0)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47497
%stackaddr$env-ref47498 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41827, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47498
%stackaddr$env-ref47499 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41827, i64 2)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47499
%stackaddr$prim47500 = alloca %struct.ScmObj*, align 8
%k40418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46381)
store volatile %struct.ScmObj* %k40418, %struct.ScmObj** %stackaddr$prim47500, align 8
%stackaddr$prim47501 = alloca %struct.ScmObj*, align 8
%current_45args46382 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46381)
store volatile %struct.ScmObj* %current_45args46382, %struct.ScmObj** %stackaddr$prim47501, align 8
%stackaddr$prim47502 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46382)
store volatile %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$prim47502, align 8
%ae41829 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47503 = alloca %struct.ScmObj*, align 8
%fptrToInt47504 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41830 to i64
%ae41830 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47504)
store volatile %struct.ScmObj* %ae41830, %struct.ScmObj** %stackaddr$makeclosure47503, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41830, %struct.ScmObj* %_37foldr40129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41830, %struct.ScmObj* %_37foldl40207, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41830, %struct.ScmObj* %_37foldr140124, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41830, %struct.ScmObj* %_37map140155, i64 3)
%args46439$k40418$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47505 = alloca %struct.ScmObj*, align 8
%args46439$k40418$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41830, %struct.ScmObj* %args46439$k40418$0)
store volatile %struct.ScmObj* %args46439$k40418$1, %struct.ScmObj** %stackaddr$prim47505, align 8
%stackaddr$prim47506 = alloca %struct.ScmObj*, align 8
%args46439$k40418$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41829, %struct.ScmObj* %args46439$k40418$1)
store volatile %struct.ScmObj* %args46439$k40418$2, %struct.ScmObj** %stackaddr$prim47506, align 8
%clofunc47507 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40418)
musttail call tailcc void %clofunc47507(%struct.ScmObj* %k40418, %struct.ScmObj* %args46439$k40418$2)
ret void
}

define tailcc void @proc_clo$ae41830(%struct.ScmObj* %env$ae41830,%struct.ScmObj* %args4020840419) {
%stackaddr$env-ref47508 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41830, i64 0)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47508
%stackaddr$env-ref47509 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41830, i64 1)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47509
%stackaddr$env-ref47510 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41830, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47510
%stackaddr$env-ref47511 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41830, i64 3)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47511
%stackaddr$prim47512 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4020840419)
store volatile %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$prim47512, align 8
%stackaddr$prim47513 = alloca %struct.ScmObj*, align 8
%args40208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4020840419)
store volatile %struct.ScmObj* %args40208, %struct.ScmObj** %stackaddr$prim47513, align 8
%stackaddr$prim47514 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40208)
store volatile %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$prim47514, align 8
%stackaddr$prim47515 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40208)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim47515, align 8
%stackaddr$prim47516 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40283)
store volatile %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$prim47516, align 8
%stackaddr$prim47517 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40208)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim47517, align 8
%stackaddr$prim47518 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40284)
store volatile %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$prim47518, align 8
%stackaddr$makeclosure47519 = alloca %struct.ScmObj*, align 8
%fptrToInt47520 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41838 to i64
%ae41838 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt47520)
store volatile %struct.ScmObj* %ae41838, %struct.ScmObj** %stackaddr$makeclosure47519, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41838, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41838, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41838, %struct.ScmObj* %k40420, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41838, %struct.ScmObj* %f40211, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41838, %struct.ScmObj* %acc40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41838, %struct.ScmObj* %_37foldl40207, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41838, %struct.ScmObj* %_37foldr140124, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41838, %struct.ScmObj* %_37map140155, i64 7)
%ae41839 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47521 = alloca %struct.ScmObj*, align 8
%fptrToInt47522 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41840 to i64
%ae41840 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47522)
store volatile %struct.ScmObj* %ae41840, %struct.ScmObj** %stackaddr$makeclosure47521, align 8
%args46438$ae41838$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47523 = alloca %struct.ScmObj*, align 8
%args46438$ae41838$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41840, %struct.ScmObj* %args46438$ae41838$0)
store volatile %struct.ScmObj* %args46438$ae41838$1, %struct.ScmObj** %stackaddr$prim47523, align 8
%stackaddr$prim47524 = alloca %struct.ScmObj*, align 8
%args46438$ae41838$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41839, %struct.ScmObj* %args46438$ae41838$1)
store volatile %struct.ScmObj* %args46438$ae41838$2, %struct.ScmObj** %stackaddr$prim47524, align 8
%clofunc47525 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41838)
musttail call tailcc void %clofunc47525(%struct.ScmObj* %ae41838, %struct.ScmObj* %args46438$ae41838$2)
ret void
}

define tailcc void @proc_clo$ae41838(%struct.ScmObj* %env$ae41838,%struct.ScmObj* %current_45args46384) {
%stackaddr$env-ref47526 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41838, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref47526
%stackaddr$env-ref47527 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41838, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47527
%stackaddr$env-ref47528 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41838, i64 2)
store %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$env-ref47528
%stackaddr$env-ref47529 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41838, i64 3)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47529
%stackaddr$env-ref47530 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41838, i64 4)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47530
%stackaddr$env-ref47531 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41838, i64 5)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47531
%stackaddr$env-ref47532 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41838, i64 6)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47532
%stackaddr$env-ref47533 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41838, i64 7)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47533
%stackaddr$prim47534 = alloca %struct.ScmObj*, align 8
%_95k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46384)
store volatile %struct.ScmObj* %_95k40421, %struct.ScmObj** %stackaddr$prim47534, align 8
%stackaddr$prim47535 = alloca %struct.ScmObj*, align 8
%current_45args46385 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46384)
store volatile %struct.ScmObj* %current_45args46385, %struct.ScmObj** %stackaddr$prim47535, align 8
%stackaddr$prim47536 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46385)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim47536, align 8
%stackaddr$makeclosure47537 = alloca %struct.ScmObj*, align 8
%fptrToInt47538 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41870 to i64
%ae41870 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47538)
store volatile %struct.ScmObj* %ae41870, %struct.ScmObj** %stackaddr$makeclosure47537, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41870, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41870, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41870, %struct.ScmObj* %k40420, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41870, %struct.ScmObj* %f40211, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41870, %struct.ScmObj* %acc40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41870, %struct.ScmObj* %_37foldl40207, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41870, %struct.ScmObj* %_37map140155, i64 6)
%ae41872 = call %struct.ScmObj* @const_init_false()
%args46431$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47539 = alloca %struct.ScmObj*, align 8
%args46431$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40209, %struct.ScmObj* %args46431$_37foldr140124$0)
store volatile %struct.ScmObj* %args46431$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim47539, align 8
%stackaddr$prim47540 = alloca %struct.ScmObj*, align 8
%args46431$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41872, %struct.ScmObj* %args46431$_37foldr140124$1)
store volatile %struct.ScmObj* %args46431$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim47540, align 8
%stackaddr$prim47541 = alloca %struct.ScmObj*, align 8
%args46431$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40285, %struct.ScmObj* %args46431$_37foldr140124$2)
store volatile %struct.ScmObj* %args46431$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim47541, align 8
%stackaddr$prim47542 = alloca %struct.ScmObj*, align 8
%args46431$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41870, %struct.ScmObj* %args46431$_37foldr140124$3)
store volatile %struct.ScmObj* %args46431$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim47542, align 8
%clofunc47543 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc47543(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args46431$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae41870(%struct.ScmObj* %env$ae41870,%struct.ScmObj* %current_45args46387) {
%stackaddr$env-ref47544 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41870, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref47544
%stackaddr$env-ref47545 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41870, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47545
%stackaddr$env-ref47546 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41870, i64 2)
store %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$env-ref47546
%stackaddr$env-ref47547 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41870, i64 3)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47547
%stackaddr$env-ref47548 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41870, i64 4)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47548
%stackaddr$env-ref47549 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41870, i64 5)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47549
%stackaddr$env-ref47550 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41870, i64 6)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47550
%stackaddr$prim47551 = alloca %struct.ScmObj*, align 8
%_95k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46387)
store volatile %struct.ScmObj* %_95k40422, %struct.ScmObj** %stackaddr$prim47551, align 8
%stackaddr$prim47552 = alloca %struct.ScmObj*, align 8
%current_45args46388 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46387)
store volatile %struct.ScmObj* %current_45args46388, %struct.ScmObj** %stackaddr$prim47552, align 8
%stackaddr$prim47553 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46388)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim47553, align 8
%truthy$cmp47554 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40286)
%cmp$cmp47554 = icmp eq i64 %truthy$cmp47554, 1
br i1 %cmp$cmp47554, label %truebranch$cmp47554, label %falsebranch$cmp47554
truebranch$cmp47554:
%ae41881 = call %struct.ScmObj* @const_init_int(i64 0)
%args46390$k40420$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47555 = alloca %struct.ScmObj*, align 8
%args46390$k40420$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40210, %struct.ScmObj* %args46390$k40420$0)
store volatile %struct.ScmObj* %args46390$k40420$1, %struct.ScmObj** %stackaddr$prim47555, align 8
%stackaddr$prim47556 = alloca %struct.ScmObj*, align 8
%args46390$k40420$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41881, %struct.ScmObj* %args46390$k40420$1)
store volatile %struct.ScmObj* %args46390$k40420$2, %struct.ScmObj** %stackaddr$prim47556, align 8
%clofunc47557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40420)
musttail call tailcc void %clofunc47557(%struct.ScmObj* %k40420, %struct.ScmObj* %args46390$k40420$2)
ret void
falsebranch$cmp47554:
%stackaddr$makeclosure47558 = alloca %struct.ScmObj*, align 8
%fptrToInt47559 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41886 to i64
%ae41886 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47559)
store volatile %struct.ScmObj* %ae41886, %struct.ScmObj** %stackaddr$makeclosure47558, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41886, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41886, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41886, %struct.ScmObj* %k40420, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41886, %struct.ScmObj* %f40211, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41886, %struct.ScmObj* %acc40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41886, %struct.ScmObj* %_37foldl40207, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41886, %struct.ScmObj* %_37map140155, i64 6)
%ae41887 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47560 = alloca %struct.ScmObj*, align 8
%fptrToInt47561 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41888 to i64
%ae41888 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47561)
store volatile %struct.ScmObj* %ae41888, %struct.ScmObj** %stackaddr$makeclosure47560, align 8
%args46430$ae41886$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47562 = alloca %struct.ScmObj*, align 8
%args46430$ae41886$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41888, %struct.ScmObj* %args46430$ae41886$0)
store volatile %struct.ScmObj* %args46430$ae41886$1, %struct.ScmObj** %stackaddr$prim47562, align 8
%stackaddr$prim47563 = alloca %struct.ScmObj*, align 8
%args46430$ae41886$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41887, %struct.ScmObj* %args46430$ae41886$1)
store volatile %struct.ScmObj* %args46430$ae41886$2, %struct.ScmObj** %stackaddr$prim47563, align 8
%clofunc47564 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41886)
musttail call tailcc void %clofunc47564(%struct.ScmObj* %ae41886, %struct.ScmObj* %args46430$ae41886$2)
ret void
}

define tailcc void @proc_clo$ae41886(%struct.ScmObj* %env$ae41886,%struct.ScmObj* %current_45args46391) {
%stackaddr$env-ref47565 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41886, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref47565
%stackaddr$env-ref47566 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41886, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47566
%stackaddr$env-ref47567 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41886, i64 2)
store %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$env-ref47567
%stackaddr$env-ref47568 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41886, i64 3)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47568
%stackaddr$env-ref47569 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41886, i64 4)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47569
%stackaddr$env-ref47570 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41886, i64 5)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47570
%stackaddr$env-ref47571 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41886, i64 6)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47571
%stackaddr$prim47572 = alloca %struct.ScmObj*, align 8
%_95k40423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46391)
store volatile %struct.ScmObj* %_95k40423, %struct.ScmObj** %stackaddr$prim47572, align 8
%stackaddr$prim47573 = alloca %struct.ScmObj*, align 8
%current_45args46392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46391)
store volatile %struct.ScmObj* %current_45args46392, %struct.ScmObj** %stackaddr$prim47573, align 8
%stackaddr$prim47574 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46392)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim47574, align 8
%stackaddr$makeclosure47575 = alloca %struct.ScmObj*, align 8
%fptrToInt47576 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41907 to i64
%ae41907 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47576)
store volatile %struct.ScmObj* %ae41907, %struct.ScmObj** %stackaddr$makeclosure47575, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %k40420, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %f40211, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %acc40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %_37foldl40207, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %_37map140155, i64 6)
%args46425$_37map140155$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47577 = alloca %struct.ScmObj*, align 8
%args46425$_37map140155$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40209, %struct.ScmObj* %args46425$_37map140155$0)
store volatile %struct.ScmObj* %args46425$_37map140155$1, %struct.ScmObj** %stackaddr$prim47577, align 8
%stackaddr$prim47578 = alloca %struct.ScmObj*, align 8
%args46425$_37map140155$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40287, %struct.ScmObj* %args46425$_37map140155$1)
store volatile %struct.ScmObj* %args46425$_37map140155$2, %struct.ScmObj** %stackaddr$prim47578, align 8
%stackaddr$prim47579 = alloca %struct.ScmObj*, align 8
%args46425$_37map140155$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41907, %struct.ScmObj* %args46425$_37map140155$2)
store volatile %struct.ScmObj* %args46425$_37map140155$3, %struct.ScmObj** %stackaddr$prim47579, align 8
%clofunc47580 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140155)
musttail call tailcc void %clofunc47580(%struct.ScmObj* %_37map140155, %struct.ScmObj* %args46425$_37map140155$3)
ret void
}

define tailcc void @proc_clo$ae41907(%struct.ScmObj* %env$ae41907,%struct.ScmObj* %current_45args46394) {
%stackaddr$env-ref47581 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref47581
%stackaddr$env-ref47582 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47582
%stackaddr$env-ref47583 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 2)
store %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$env-ref47583
%stackaddr$env-ref47584 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 3)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47584
%stackaddr$env-ref47585 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 4)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47585
%stackaddr$env-ref47586 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 5)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47586
%stackaddr$env-ref47587 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 6)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47587
%stackaddr$prim47588 = alloca %struct.ScmObj*, align 8
%_95k40424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46394)
store volatile %struct.ScmObj* %_95k40424, %struct.ScmObj** %stackaddr$prim47588, align 8
%stackaddr$prim47589 = alloca %struct.ScmObj*, align 8
%current_45args46395 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46394)
store volatile %struct.ScmObj* %current_45args46395, %struct.ScmObj** %stackaddr$prim47589, align 8
%stackaddr$prim47590 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46395)
store volatile %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$prim47590, align 8
%stackaddr$makeclosure47591 = alloca %struct.ScmObj*, align 8
%fptrToInt47592 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41910 to i64
%ae41910 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt47592)
store volatile %struct.ScmObj* %ae41910, %struct.ScmObj** %stackaddr$makeclosure47591, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %lsts_4340216, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %k40420, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %f40211, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %acc40210, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %_37foldl40207, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %_37map140155, i64 7)
%ae41911 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47593 = alloca %struct.ScmObj*, align 8
%fptrToInt47594 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41912 to i64
%ae41912 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47594)
store volatile %struct.ScmObj* %ae41912, %struct.ScmObj** %stackaddr$makeclosure47593, align 8
%args46424$ae41910$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47595 = alloca %struct.ScmObj*, align 8
%args46424$ae41910$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41912, %struct.ScmObj* %args46424$ae41910$0)
store volatile %struct.ScmObj* %args46424$ae41910$1, %struct.ScmObj** %stackaddr$prim47595, align 8
%stackaddr$prim47596 = alloca %struct.ScmObj*, align 8
%args46424$ae41910$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41911, %struct.ScmObj* %args46424$ae41910$1)
store volatile %struct.ScmObj* %args46424$ae41910$2, %struct.ScmObj** %stackaddr$prim47596, align 8
%clofunc47597 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41910)
musttail call tailcc void %clofunc47597(%struct.ScmObj* %ae41910, %struct.ScmObj* %args46424$ae41910$2)
ret void
}

define tailcc void @proc_clo$ae41910(%struct.ScmObj* %env$ae41910,%struct.ScmObj* %current_45args46397) {
%stackaddr$env-ref47598 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref47598
%stackaddr$env-ref47599 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47599
%stackaddr$env-ref47600 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 2)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref47600
%stackaddr$env-ref47601 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 3)
store %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$env-ref47601
%stackaddr$env-ref47602 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 4)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47602
%stackaddr$env-ref47603 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 5)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47603
%stackaddr$env-ref47604 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 6)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47604
%stackaddr$env-ref47605 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 7)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47605
%stackaddr$prim47606 = alloca %struct.ScmObj*, align 8
%_95k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46397)
store volatile %struct.ScmObj* %_95k40425, %struct.ScmObj** %stackaddr$prim47606, align 8
%stackaddr$prim47607 = alloca %struct.ScmObj*, align 8
%current_45args46398 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46397)
store volatile %struct.ScmObj* %current_45args46398, %struct.ScmObj** %stackaddr$prim47607, align 8
%stackaddr$prim47608 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46398)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim47608, align 8
%stackaddr$makeclosure47609 = alloca %struct.ScmObj*, align 8
%fptrToInt47610 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41931 to i64
%ae41931 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47610)
store volatile %struct.ScmObj* %ae41931, %struct.ScmObj** %stackaddr$makeclosure47609, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %lsts_4340216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %k40420, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %f40211, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %acc40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %_37foldl40207, i64 5)
%args46419$_37map140155$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47611 = alloca %struct.ScmObj*, align 8
%args46419$_37map140155$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40209, %struct.ScmObj* %args46419$_37map140155$0)
store volatile %struct.ScmObj* %args46419$_37map140155$1, %struct.ScmObj** %stackaddr$prim47611, align 8
%stackaddr$prim47612 = alloca %struct.ScmObj*, align 8
%args46419$_37map140155$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40288, %struct.ScmObj* %args46419$_37map140155$1)
store volatile %struct.ScmObj* %args46419$_37map140155$2, %struct.ScmObj** %stackaddr$prim47612, align 8
%stackaddr$prim47613 = alloca %struct.ScmObj*, align 8
%args46419$_37map140155$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41931, %struct.ScmObj* %args46419$_37map140155$2)
store volatile %struct.ScmObj* %args46419$_37map140155$3, %struct.ScmObj** %stackaddr$prim47613, align 8
%clofunc47614 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140155)
musttail call tailcc void %clofunc47614(%struct.ScmObj* %_37map140155, %struct.ScmObj* %args46419$_37map140155$3)
ret void
}

define tailcc void @proc_clo$ae41931(%struct.ScmObj* %env$ae41931,%struct.ScmObj* %current_45args46400) {
%stackaddr$env-ref47615 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 0)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref47615
%stackaddr$env-ref47616 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 1)
store %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$env-ref47616
%stackaddr$env-ref47617 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 2)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47617
%stackaddr$env-ref47618 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 3)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47618
%stackaddr$env-ref47619 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47619
%stackaddr$env-ref47620 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 5)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47620
%stackaddr$prim47621 = alloca %struct.ScmObj*, align 8
%_95k40426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46400)
store volatile %struct.ScmObj* %_95k40426, %struct.ScmObj** %stackaddr$prim47621, align 8
%stackaddr$prim47622 = alloca %struct.ScmObj*, align 8
%current_45args46401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46400)
store volatile %struct.ScmObj* %current_45args46401, %struct.ScmObj** %stackaddr$prim47622, align 8
%stackaddr$prim47623 = alloca %struct.ScmObj*, align 8
%vs40214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46401)
store volatile %struct.ScmObj* %vs40214, %struct.ScmObj** %stackaddr$prim47623, align 8
%stackaddr$makeclosure47624 = alloca %struct.ScmObj*, align 8
%fptrToInt47625 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41934 to i64
%ae41934 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47625)
store volatile %struct.ScmObj* %ae41934, %struct.ScmObj** %stackaddr$makeclosure47624, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %lsts_4340216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %vs40214, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %k40420, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %f40211, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %acc40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %_37foldl40207, i64 6)
%ae41935 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47626 = alloca %struct.ScmObj*, align 8
%fptrToInt47627 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41936 to i64
%ae41936 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47627)
store volatile %struct.ScmObj* %ae41936, %struct.ScmObj** %stackaddr$makeclosure47626, align 8
%args46418$ae41934$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47628 = alloca %struct.ScmObj*, align 8
%args46418$ae41934$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41936, %struct.ScmObj* %args46418$ae41934$0)
store volatile %struct.ScmObj* %args46418$ae41934$1, %struct.ScmObj** %stackaddr$prim47628, align 8
%stackaddr$prim47629 = alloca %struct.ScmObj*, align 8
%args46418$ae41934$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41935, %struct.ScmObj* %args46418$ae41934$1)
store volatile %struct.ScmObj* %args46418$ae41934$2, %struct.ScmObj** %stackaddr$prim47629, align 8
%clofunc47630 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41934)
musttail call tailcc void %clofunc47630(%struct.ScmObj* %ae41934, %struct.ScmObj* %args46418$ae41934$2)
ret void
}

define tailcc void @proc_clo$ae41934(%struct.ScmObj* %env$ae41934,%struct.ScmObj* %current_45args46403) {
%stackaddr$env-ref47631 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 0)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref47631
%stackaddr$env-ref47632 = alloca %struct.ScmObj*, align 8
%vs40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 1)
store %struct.ScmObj* %vs40214, %struct.ScmObj** %stackaddr$env-ref47632
%stackaddr$env-ref47633 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 2)
store %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$env-ref47633
%stackaddr$env-ref47634 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 3)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47634
%stackaddr$env-ref47635 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 4)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47635
%stackaddr$env-ref47636 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47636
%stackaddr$env-ref47637 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 6)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47637
%stackaddr$prim47638 = alloca %struct.ScmObj*, align 8
%_95k40427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46403)
store volatile %struct.ScmObj* %_95k40427, %struct.ScmObj** %stackaddr$prim47638, align 8
%stackaddr$prim47639 = alloca %struct.ScmObj*, align 8
%current_45args46404 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46403)
store volatile %struct.ScmObj* %current_45args46404, %struct.ScmObj** %stackaddr$prim47639, align 8
%stackaddr$prim47640 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46404)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim47640, align 8
%ae41957 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47641 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40210, %struct.ScmObj* %ae41957)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim47641, align 8
%stackaddr$makeclosure47642 = alloca %struct.ScmObj*, align 8
%fptrToInt47643 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41959 to i64
%ae41959 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47643)
store volatile %struct.ScmObj* %ae41959, %struct.ScmObj** %stackaddr$makeclosure47642, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41959, %struct.ScmObj* %lsts_4340216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41959, %struct.ScmObj* %k40420, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41959, %struct.ScmObj* %f40211, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41959, %struct.ScmObj* %_37foldl40207, i64 3)
%args46412$_37foldr40129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47644 = alloca %struct.ScmObj*, align 8
%args46412$_37foldr40129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40214, %struct.ScmObj* %args46412$_37foldr40129$0)
store volatile %struct.ScmObj* %args46412$_37foldr40129$1, %struct.ScmObj** %stackaddr$prim47644, align 8
%stackaddr$prim47645 = alloca %struct.ScmObj*, align 8
%args46412$_37foldr40129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40290, %struct.ScmObj* %args46412$_37foldr40129$1)
store volatile %struct.ScmObj* %args46412$_37foldr40129$2, %struct.ScmObj** %stackaddr$prim47645, align 8
%stackaddr$prim47646 = alloca %struct.ScmObj*, align 8
%args46412$_37foldr40129$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40289, %struct.ScmObj* %args46412$_37foldr40129$2)
store volatile %struct.ScmObj* %args46412$_37foldr40129$3, %struct.ScmObj** %stackaddr$prim47646, align 8
%stackaddr$prim47647 = alloca %struct.ScmObj*, align 8
%args46412$_37foldr40129$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41959, %struct.ScmObj* %args46412$_37foldr40129$3)
store volatile %struct.ScmObj* %args46412$_37foldr40129$4, %struct.ScmObj** %stackaddr$prim47647, align 8
%clofunc47648 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40129)
musttail call tailcc void %clofunc47648(%struct.ScmObj* %_37foldr40129, %struct.ScmObj* %args46412$_37foldr40129$4)
ret void
}

define tailcc void @proc_clo$ae41959(%struct.ScmObj* %env$ae41959,%struct.ScmObj* %current_45args46406) {
%stackaddr$env-ref47649 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41959, i64 0)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref47649
%stackaddr$env-ref47650 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41959, i64 1)
store %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$env-ref47650
%stackaddr$env-ref47651 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41959, i64 2)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47651
%stackaddr$env-ref47652 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41959, i64 3)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47652
%stackaddr$prim47653 = alloca %struct.ScmObj*, align 8
%_95k40428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46406)
store volatile %struct.ScmObj* %_95k40428, %struct.ScmObj** %stackaddr$prim47653, align 8
%stackaddr$prim47654 = alloca %struct.ScmObj*, align 8
%current_45args46407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46406)
store volatile %struct.ScmObj* %current_45args46407, %struct.ScmObj** %stackaddr$prim47654, align 8
%stackaddr$prim47655 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46407)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim47655, align 8
%stackaddr$makeclosure47656 = alloca %struct.ScmObj*, align 8
%fptrToInt47657 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41963 to i64
%ae41963 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47657)
store volatile %struct.ScmObj* %ae41963, %struct.ScmObj** %stackaddr$makeclosure47656, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41963, %struct.ScmObj* %lsts_4340216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41963, %struct.ScmObj* %k40420, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41963, %struct.ScmObj* %f40211, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41963, %struct.ScmObj* %_37foldl40207, i64 3)
%stackaddr$prim47658 = alloca %struct.ScmObj*, align 8
%cpsargs40431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41963, %struct.ScmObj* %anf_45bind40291)
store volatile %struct.ScmObj* %cpsargs40431, %struct.ScmObj** %stackaddr$prim47658, align 8
%clofunc47659 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40211)
musttail call tailcc void %clofunc47659(%struct.ScmObj* %f40211, %struct.ScmObj* %cpsargs40431)
ret void
}

define tailcc void @proc_clo$ae41963(%struct.ScmObj* %env$ae41963,%struct.ScmObj* %current_45args46409) {
%stackaddr$env-ref47660 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41963, i64 0)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref47660
%stackaddr$env-ref47661 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41963, i64 1)
store %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$env-ref47661
%stackaddr$env-ref47662 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41963, i64 2)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47662
%stackaddr$env-ref47663 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41963, i64 3)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47663
%stackaddr$prim47664 = alloca %struct.ScmObj*, align 8
%_95k40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46409)
store volatile %struct.ScmObj* %_95k40429, %struct.ScmObj** %stackaddr$prim47664, align 8
%stackaddr$prim47665 = alloca %struct.ScmObj*, align 8
%current_45args46410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46409)
store volatile %struct.ScmObj* %current_45args46410, %struct.ScmObj** %stackaddr$prim47665, align 8
%stackaddr$prim47666 = alloca %struct.ScmObj*, align 8
%acc_4340218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46410)
store volatile %struct.ScmObj* %acc_4340218, %struct.ScmObj** %stackaddr$prim47666, align 8
%stackaddr$prim47667 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340218, %struct.ScmObj* %lsts_4340216)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim47667, align 8
%stackaddr$prim47668 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40211, %struct.ScmObj* %anf_45bind40292)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim47668, align 8
%stackaddr$prim47669 = alloca %struct.ScmObj*, align 8
%cpsargs40430 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40420, %struct.ScmObj* %anf_45bind40293)
store volatile %struct.ScmObj* %cpsargs40430, %struct.ScmObj** %stackaddr$prim47669, align 8
%clofunc47670 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40207)
musttail call tailcc void %clofunc47670(%struct.ScmObj* %_37foldl40207, %struct.ScmObj* %cpsargs40430)
ret void
}

define tailcc void @proc_clo$ae41936(%struct.ScmObj* %env$ae41936,%struct.ScmObj* %current_45args46413) {
%stackaddr$prim47671 = alloca %struct.ScmObj*, align 8
%k40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46413)
store volatile %struct.ScmObj* %k40432, %struct.ScmObj** %stackaddr$prim47671, align 8
%stackaddr$prim47672 = alloca %struct.ScmObj*, align 8
%current_45args46414 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46413)
store volatile %struct.ScmObj* %current_45args46414, %struct.ScmObj** %stackaddr$prim47672, align 8
%stackaddr$prim47673 = alloca %struct.ScmObj*, align 8
%a40220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46414)
store volatile %struct.ScmObj* %a40220, %struct.ScmObj** %stackaddr$prim47673, align 8
%stackaddr$prim47674 = alloca %struct.ScmObj*, align 8
%current_45args46415 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46414)
store volatile %struct.ScmObj* %current_45args46415, %struct.ScmObj** %stackaddr$prim47674, align 8
%stackaddr$prim47675 = alloca %struct.ScmObj*, align 8
%b40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46415)
store volatile %struct.ScmObj* %b40219, %struct.ScmObj** %stackaddr$prim47675, align 8
%stackaddr$prim47676 = alloca %struct.ScmObj*, align 8
%cpsprim40433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40220, %struct.ScmObj* %b40219)
store volatile %struct.ScmObj* %cpsprim40433, %struct.ScmObj** %stackaddr$prim47676, align 8
%ae41940 = call %struct.ScmObj* @const_init_int(i64 0)
%args46417$k40432$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47677 = alloca %struct.ScmObj*, align 8
%args46417$k40432$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40433, %struct.ScmObj* %args46417$k40432$0)
store volatile %struct.ScmObj* %args46417$k40432$1, %struct.ScmObj** %stackaddr$prim47677, align 8
%stackaddr$prim47678 = alloca %struct.ScmObj*, align 8
%args46417$k40432$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41940, %struct.ScmObj* %args46417$k40432$1)
store volatile %struct.ScmObj* %args46417$k40432$2, %struct.ScmObj** %stackaddr$prim47678, align 8
%clofunc47679 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40432)
musttail call tailcc void %clofunc47679(%struct.ScmObj* %k40432, %struct.ScmObj* %args46417$k40432$2)
ret void
}

define tailcc void @proc_clo$ae41912(%struct.ScmObj* %env$ae41912,%struct.ScmObj* %current_45args46420) {
%stackaddr$prim47680 = alloca %struct.ScmObj*, align 8
%k40434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46420)
store volatile %struct.ScmObj* %k40434, %struct.ScmObj** %stackaddr$prim47680, align 8
%stackaddr$prim47681 = alloca %struct.ScmObj*, align 8
%current_45args46421 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46420)
store volatile %struct.ScmObj* %current_45args46421, %struct.ScmObj** %stackaddr$prim47681, align 8
%stackaddr$prim47682 = alloca %struct.ScmObj*, align 8
%x40215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46421)
store volatile %struct.ScmObj* %x40215, %struct.ScmObj** %stackaddr$prim47682, align 8
%stackaddr$prim47683 = alloca %struct.ScmObj*, align 8
%cpsprim40435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40215)
store volatile %struct.ScmObj* %cpsprim40435, %struct.ScmObj** %stackaddr$prim47683, align 8
%ae41915 = call %struct.ScmObj* @const_init_int(i64 0)
%args46423$k40434$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47684 = alloca %struct.ScmObj*, align 8
%args46423$k40434$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40435, %struct.ScmObj* %args46423$k40434$0)
store volatile %struct.ScmObj* %args46423$k40434$1, %struct.ScmObj** %stackaddr$prim47684, align 8
%stackaddr$prim47685 = alloca %struct.ScmObj*, align 8
%args46423$k40434$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41915, %struct.ScmObj* %args46423$k40434$1)
store volatile %struct.ScmObj* %args46423$k40434$2, %struct.ScmObj** %stackaddr$prim47685, align 8
%clofunc47686 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40434)
musttail call tailcc void %clofunc47686(%struct.ScmObj* %k40434, %struct.ScmObj* %args46423$k40434$2)
ret void
}

define tailcc void @proc_clo$ae41888(%struct.ScmObj* %env$ae41888,%struct.ScmObj* %current_45args46426) {
%stackaddr$prim47687 = alloca %struct.ScmObj*, align 8
%k40436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46426)
store volatile %struct.ScmObj* %k40436, %struct.ScmObj** %stackaddr$prim47687, align 8
%stackaddr$prim47688 = alloca %struct.ScmObj*, align 8
%current_45args46427 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46426)
store volatile %struct.ScmObj* %current_45args46427, %struct.ScmObj** %stackaddr$prim47688, align 8
%stackaddr$prim47689 = alloca %struct.ScmObj*, align 8
%x40217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46427)
store volatile %struct.ScmObj* %x40217, %struct.ScmObj** %stackaddr$prim47689, align 8
%stackaddr$prim47690 = alloca %struct.ScmObj*, align 8
%cpsprim40437 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40217)
store volatile %struct.ScmObj* %cpsprim40437, %struct.ScmObj** %stackaddr$prim47690, align 8
%ae41891 = call %struct.ScmObj* @const_init_int(i64 0)
%args46429$k40436$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47691 = alloca %struct.ScmObj*, align 8
%args46429$k40436$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40437, %struct.ScmObj* %args46429$k40436$0)
store volatile %struct.ScmObj* %args46429$k40436$1, %struct.ScmObj** %stackaddr$prim47691, align 8
%stackaddr$prim47692 = alloca %struct.ScmObj*, align 8
%args46429$k40436$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41891, %struct.ScmObj* %args46429$k40436$1)
store volatile %struct.ScmObj* %args46429$k40436$2, %struct.ScmObj** %stackaddr$prim47692, align 8
%clofunc47693 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40436)
musttail call tailcc void %clofunc47693(%struct.ScmObj* %k40436, %struct.ScmObj* %args46429$k40436$2)
ret void
}

define tailcc void @proc_clo$ae41840(%struct.ScmObj* %env$ae41840,%struct.ScmObj* %current_45args46432) {
%stackaddr$prim47694 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46432)
store volatile %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$prim47694, align 8
%stackaddr$prim47695 = alloca %struct.ScmObj*, align 8
%current_45args46433 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46432)
store volatile %struct.ScmObj* %current_45args46433, %struct.ScmObj** %stackaddr$prim47695, align 8
%stackaddr$prim47696 = alloca %struct.ScmObj*, align 8
%lst40213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46433)
store volatile %struct.ScmObj* %lst40213, %struct.ScmObj** %stackaddr$prim47696, align 8
%stackaddr$prim47697 = alloca %struct.ScmObj*, align 8
%current_45args46434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46433)
store volatile %struct.ScmObj* %current_45args46434, %struct.ScmObj** %stackaddr$prim47697, align 8
%stackaddr$prim47698 = alloca %struct.ScmObj*, align 8
%b40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46434)
store volatile %struct.ScmObj* %b40212, %struct.ScmObj** %stackaddr$prim47698, align 8
%truthy$cmp47699 = call i64 @is_truthy_value(%struct.ScmObj* %b40212)
%cmp$cmp47699 = icmp eq i64 %truthy$cmp47699, 1
br i1 %cmp$cmp47699, label %truebranch$cmp47699, label %falsebranch$cmp47699
truebranch$cmp47699:
%ae41843 = call %struct.ScmObj* @const_init_int(i64 0)
%args46436$k40438$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47700 = alloca %struct.ScmObj*, align 8
%args46436$k40438$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40212, %struct.ScmObj* %args46436$k40438$0)
store volatile %struct.ScmObj* %args46436$k40438$1, %struct.ScmObj** %stackaddr$prim47700, align 8
%stackaddr$prim47701 = alloca %struct.ScmObj*, align 8
%args46436$k40438$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41843, %struct.ScmObj* %args46436$k40438$1)
store volatile %struct.ScmObj* %args46436$k40438$2, %struct.ScmObj** %stackaddr$prim47701, align 8
%clofunc47702 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40438)
musttail call tailcc void %clofunc47702(%struct.ScmObj* %k40438, %struct.ScmObj* %args46436$k40438$2)
ret void
falsebranch$cmp47699:
%stackaddr$prim47703 = alloca %struct.ScmObj*, align 8
%cpsprim40439 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40213)
store volatile %struct.ScmObj* %cpsprim40439, %struct.ScmObj** %stackaddr$prim47703, align 8
%ae41850 = call %struct.ScmObj* @const_init_int(i64 0)
%args46437$k40438$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47704 = alloca %struct.ScmObj*, align 8
%args46437$k40438$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40439, %struct.ScmObj* %args46437$k40438$0)
store volatile %struct.ScmObj* %args46437$k40438$1, %struct.ScmObj** %stackaddr$prim47704, align 8
%stackaddr$prim47705 = alloca %struct.ScmObj*, align 8
%args46437$k40438$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41850, %struct.ScmObj* %args46437$k40438$1)
store volatile %struct.ScmObj* %args46437$k40438$2, %struct.ScmObj** %stackaddr$prim47705, align 8
%clofunc47706 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40438)
musttail call tailcc void %clofunc47706(%struct.ScmObj* %k40438, %struct.ScmObj* %args46437$k40438$2)
ret void
}

define tailcc void @proc_clo$ae41681(%struct.ScmObj* %env$ae41681,%struct.ScmObj* %args4015140440) {
%stackaddr$env-ref47707 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41681, i64 0)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref47707
%stackaddr$env-ref47708 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41681, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47708
%stackaddr$env-ref47709 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41681, i64 2)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref47709
%stackaddr$prim47710 = alloca %struct.ScmObj*, align 8
%k40441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015140440)
store volatile %struct.ScmObj* %k40441, %struct.ScmObj** %stackaddr$prim47710, align 8
%stackaddr$prim47711 = alloca %struct.ScmObj*, align 8
%args40151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015140440)
store volatile %struct.ScmObj* %args40151, %struct.ScmObj** %stackaddr$prim47711, align 8
%stackaddr$prim47712 = alloca %struct.ScmObj*, align 8
%f40153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40151)
store volatile %struct.ScmObj* %f40153, %struct.ScmObj** %stackaddr$prim47712, align 8
%stackaddr$prim47713 = alloca %struct.ScmObj*, align 8
%lsts40152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40151)
store volatile %struct.ScmObj* %lsts40152, %struct.ScmObj** %stackaddr$prim47713, align 8
%stackaddr$makeclosure47714 = alloca %struct.ScmObj*, align 8
%fptrToInt47715 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41686 to i64
%ae41686 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47715)
store volatile %struct.ScmObj* %ae41686, %struct.ScmObj** %stackaddr$makeclosure47714, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41686, %struct.ScmObj* %k40441, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41686, %struct.ScmObj* %lsts40152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41686, %struct.ScmObj* %_37foldr40129, i64 2)
%ae41687 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47716 = alloca %struct.ScmObj*, align 8
%fptrToInt47717 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41688 to i64
%ae41688 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47717)
store volatile %struct.ScmObj* %ae41688, %struct.ScmObj** %stackaddr$makeclosure47716, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41688, %struct.ScmObj* %f40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41688, %struct.ScmObj* %_37last40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41688, %struct.ScmObj* %_37drop_45right40143, i64 2)
%args46456$ae41686$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47718 = alloca %struct.ScmObj*, align 8
%args46456$ae41686$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41688, %struct.ScmObj* %args46456$ae41686$0)
store volatile %struct.ScmObj* %args46456$ae41686$1, %struct.ScmObj** %stackaddr$prim47718, align 8
%stackaddr$prim47719 = alloca %struct.ScmObj*, align 8
%args46456$ae41686$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41687, %struct.ScmObj* %args46456$ae41686$1)
store volatile %struct.ScmObj* %args46456$ae41686$2, %struct.ScmObj** %stackaddr$prim47719, align 8
%clofunc47720 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41686)
musttail call tailcc void %clofunc47720(%struct.ScmObj* %ae41686, %struct.ScmObj* %args46456$ae41686$2)
ret void
}

define tailcc void @proc_clo$ae41686(%struct.ScmObj* %env$ae41686,%struct.ScmObj* %current_45args46441) {
%stackaddr$env-ref47721 = alloca %struct.ScmObj*, align 8
%k40441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41686, i64 0)
store %struct.ScmObj* %k40441, %struct.ScmObj** %stackaddr$env-ref47721
%stackaddr$env-ref47722 = alloca %struct.ScmObj*, align 8
%lsts40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41686, i64 1)
store %struct.ScmObj* %lsts40152, %struct.ScmObj** %stackaddr$env-ref47722
%stackaddr$env-ref47723 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41686, i64 2)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47723
%stackaddr$prim47724 = alloca %struct.ScmObj*, align 8
%_95k40442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46441)
store volatile %struct.ScmObj* %_95k40442, %struct.ScmObj** %stackaddr$prim47724, align 8
%stackaddr$prim47725 = alloca %struct.ScmObj*, align 8
%current_45args46442 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46441)
store volatile %struct.ScmObj* %current_45args46442, %struct.ScmObj** %stackaddr$prim47725, align 8
%stackaddr$prim47726 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46442)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim47726, align 8
%ae41749 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47727 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41749, %struct.ScmObj* %lsts40152)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim47727, align 8
%stackaddr$prim47728 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40280, %struct.ScmObj* %anf_45bind40281)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim47728, align 8
%stackaddr$prim47729 = alloca %struct.ScmObj*, align 8
%cpsargs40443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40441, %struct.ScmObj* %anf_45bind40282)
store volatile %struct.ScmObj* %cpsargs40443, %struct.ScmObj** %stackaddr$prim47729, align 8
%clofunc47730 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40129)
musttail call tailcc void %clofunc47730(%struct.ScmObj* %_37foldr40129, %struct.ScmObj* %cpsargs40443)
ret void
}

define tailcc void @proc_clo$ae41688(%struct.ScmObj* %env$ae41688,%struct.ScmObj* %fargs4015440444) {
%stackaddr$env-ref47731 = alloca %struct.ScmObj*, align 8
%f40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41688, i64 0)
store %struct.ScmObj* %f40153, %struct.ScmObj** %stackaddr$env-ref47731
%stackaddr$env-ref47732 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41688, i64 1)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref47732
%stackaddr$env-ref47733 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41688, i64 2)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref47733
%stackaddr$prim47734 = alloca %struct.ScmObj*, align 8
%k40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4015440444)
store volatile %struct.ScmObj* %k40445, %struct.ScmObj** %stackaddr$prim47734, align 8
%stackaddr$prim47735 = alloca %struct.ScmObj*, align 8
%fargs40154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4015440444)
store volatile %struct.ScmObj* %fargs40154, %struct.ScmObj** %stackaddr$prim47735, align 8
%stackaddr$makeclosure47736 = alloca %struct.ScmObj*, align 8
%fptrToInt47737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41692 to i64
%ae41692 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47737)
store volatile %struct.ScmObj* %ae41692, %struct.ScmObj** %stackaddr$makeclosure47736, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41692, %struct.ScmObj* %f40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41692, %struct.ScmObj* %_37last40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41692, %struct.ScmObj* %k40445, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41692, %struct.ScmObj* %fargs40154, i64 3)
%ae41694 = call %struct.ScmObj* @const_init_int(i64 1)
%args46455$_37drop_45right40143$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47738 = alloca %struct.ScmObj*, align 8
%args46455$_37drop_45right40143$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41694, %struct.ScmObj* %args46455$_37drop_45right40143$0)
store volatile %struct.ScmObj* %args46455$_37drop_45right40143$1, %struct.ScmObj** %stackaddr$prim47738, align 8
%stackaddr$prim47739 = alloca %struct.ScmObj*, align 8
%args46455$_37drop_45right40143$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40154, %struct.ScmObj* %args46455$_37drop_45right40143$1)
store volatile %struct.ScmObj* %args46455$_37drop_45right40143$2, %struct.ScmObj** %stackaddr$prim47739, align 8
%stackaddr$prim47740 = alloca %struct.ScmObj*, align 8
%args46455$_37drop_45right40143$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41692, %struct.ScmObj* %args46455$_37drop_45right40143$2)
store volatile %struct.ScmObj* %args46455$_37drop_45right40143$3, %struct.ScmObj** %stackaddr$prim47740, align 8
%clofunc47741 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40143)
musttail call tailcc void %clofunc47741(%struct.ScmObj* %_37drop_45right40143, %struct.ScmObj* %args46455$_37drop_45right40143$3)
ret void
}

define tailcc void @proc_clo$ae41692(%struct.ScmObj* %env$ae41692,%struct.ScmObj* %current_45args46444) {
%stackaddr$env-ref47742 = alloca %struct.ScmObj*, align 8
%f40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41692, i64 0)
store %struct.ScmObj* %f40153, %struct.ScmObj** %stackaddr$env-ref47742
%stackaddr$env-ref47743 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41692, i64 1)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref47743
%stackaddr$env-ref47744 = alloca %struct.ScmObj*, align 8
%k40445 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41692, i64 2)
store %struct.ScmObj* %k40445, %struct.ScmObj** %stackaddr$env-ref47744
%stackaddr$env-ref47745 = alloca %struct.ScmObj*, align 8
%fargs40154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41692, i64 3)
store %struct.ScmObj* %fargs40154, %struct.ScmObj** %stackaddr$env-ref47745
%stackaddr$prim47746 = alloca %struct.ScmObj*, align 8
%_95k40446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46444)
store volatile %struct.ScmObj* %_95k40446, %struct.ScmObj** %stackaddr$prim47746, align 8
%stackaddr$prim47747 = alloca %struct.ScmObj*, align 8
%current_45args46445 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46444)
store volatile %struct.ScmObj* %current_45args46445, %struct.ScmObj** %stackaddr$prim47747, align 8
%stackaddr$prim47748 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46445)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim47748, align 8
%stackaddr$makeclosure47749 = alloca %struct.ScmObj*, align 8
%fptrToInt47750 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41699 to i64
%ae41699 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47750)
store volatile %struct.ScmObj* %ae41699, %struct.ScmObj** %stackaddr$makeclosure47749, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41699, %struct.ScmObj* %_37last40146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41699, %struct.ScmObj* %k40445, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41699, %struct.ScmObj* %fargs40154, i64 2)
%stackaddr$prim47751 = alloca %struct.ScmObj*, align 8
%cpsargs40450 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41699, %struct.ScmObj* %anf_45bind40277)
store volatile %struct.ScmObj* %cpsargs40450, %struct.ScmObj** %stackaddr$prim47751, align 8
%clofunc47752 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40153)
musttail call tailcc void %clofunc47752(%struct.ScmObj* %f40153, %struct.ScmObj* %cpsargs40450)
ret void
}

define tailcc void @proc_clo$ae41699(%struct.ScmObj* %env$ae41699,%struct.ScmObj* %current_45args46447) {
%stackaddr$env-ref47753 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41699, i64 0)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref47753
%stackaddr$env-ref47754 = alloca %struct.ScmObj*, align 8
%k40445 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41699, i64 1)
store %struct.ScmObj* %k40445, %struct.ScmObj** %stackaddr$env-ref47754
%stackaddr$env-ref47755 = alloca %struct.ScmObj*, align 8
%fargs40154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41699, i64 2)
store %struct.ScmObj* %fargs40154, %struct.ScmObj** %stackaddr$env-ref47755
%stackaddr$prim47756 = alloca %struct.ScmObj*, align 8
%_95k40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46447)
store volatile %struct.ScmObj* %_95k40447, %struct.ScmObj** %stackaddr$prim47756, align 8
%stackaddr$prim47757 = alloca %struct.ScmObj*, align 8
%current_45args46448 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46447)
store volatile %struct.ScmObj* %current_45args46448, %struct.ScmObj** %stackaddr$prim47757, align 8
%stackaddr$prim47758 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46448)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim47758, align 8
%stackaddr$makeclosure47759 = alloca %struct.ScmObj*, align 8
%fptrToInt47760 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41704 to i64
%ae41704 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47760)
store volatile %struct.ScmObj* %ae41704, %struct.ScmObj** %stackaddr$makeclosure47759, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41704, %struct.ScmObj* %anf_45bind40278, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41704, %struct.ScmObj* %k40445, i64 1)
%args46454$_37last40146$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47761 = alloca %struct.ScmObj*, align 8
%args46454$_37last40146$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40154, %struct.ScmObj* %args46454$_37last40146$0)
store volatile %struct.ScmObj* %args46454$_37last40146$1, %struct.ScmObj** %stackaddr$prim47761, align 8
%stackaddr$prim47762 = alloca %struct.ScmObj*, align 8
%args46454$_37last40146$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41704, %struct.ScmObj* %args46454$_37last40146$1)
store volatile %struct.ScmObj* %args46454$_37last40146$2, %struct.ScmObj** %stackaddr$prim47762, align 8
%clofunc47763 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40146)
musttail call tailcc void %clofunc47763(%struct.ScmObj* %_37last40146, %struct.ScmObj* %args46454$_37last40146$2)
ret void
}

define tailcc void @proc_clo$ae41704(%struct.ScmObj* %env$ae41704,%struct.ScmObj* %current_45args46450) {
%stackaddr$env-ref47764 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41704, i64 0)
store %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$env-ref47764
%stackaddr$env-ref47765 = alloca %struct.ScmObj*, align 8
%k40445 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41704, i64 1)
store %struct.ScmObj* %k40445, %struct.ScmObj** %stackaddr$env-ref47765
%stackaddr$prim47766 = alloca %struct.ScmObj*, align 8
%_95k40448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46450)
store volatile %struct.ScmObj* %_95k40448, %struct.ScmObj** %stackaddr$prim47766, align 8
%stackaddr$prim47767 = alloca %struct.ScmObj*, align 8
%current_45args46451 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46450)
store volatile %struct.ScmObj* %current_45args46451, %struct.ScmObj** %stackaddr$prim47767, align 8
%stackaddr$prim47768 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46451)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim47768, align 8
%stackaddr$prim47769 = alloca %struct.ScmObj*, align 8
%cpsprim40449 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40278, %struct.ScmObj* %anf_45bind40279)
store volatile %struct.ScmObj* %cpsprim40449, %struct.ScmObj** %stackaddr$prim47769, align 8
%ae41709 = call %struct.ScmObj* @const_init_int(i64 0)
%args46453$k40445$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47770 = alloca %struct.ScmObj*, align 8
%args46453$k40445$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40449, %struct.ScmObj* %args46453$k40445$0)
store volatile %struct.ScmObj* %args46453$k40445$1, %struct.ScmObj** %stackaddr$prim47770, align 8
%stackaddr$prim47771 = alloca %struct.ScmObj*, align 8
%args46453$k40445$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41709, %struct.ScmObj* %args46453$k40445$1)
store volatile %struct.ScmObj* %args46453$k40445$2, %struct.ScmObj** %stackaddr$prim47771, align 8
%clofunc47772 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40445)
musttail call tailcc void %clofunc47772(%struct.ScmObj* %k40445, %struct.ScmObj* %args46453$k40445$2)
ret void
}

define tailcc void @proc_clo$ae41604(%struct.ScmObj* %env$ae41604,%struct.ScmObj* %current_45args46458) {
%stackaddr$env-ref47773 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41604, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47773
%stackaddr$prim47774 = alloca %struct.ScmObj*, align 8
%k40451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46458)
store volatile %struct.ScmObj* %k40451, %struct.ScmObj** %stackaddr$prim47774, align 8
%stackaddr$prim47775 = alloca %struct.ScmObj*, align 8
%current_45args46459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46458)
store volatile %struct.ScmObj* %current_45args46459, %struct.ScmObj** %stackaddr$prim47775, align 8
%stackaddr$prim47776 = alloca %struct.ScmObj*, align 8
%f40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46459)
store volatile %struct.ScmObj* %f40157, %struct.ScmObj** %stackaddr$prim47776, align 8
%stackaddr$prim47777 = alloca %struct.ScmObj*, align 8
%current_45args46460 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46459)
store volatile %struct.ScmObj* %current_45args46460, %struct.ScmObj** %stackaddr$prim47777, align 8
%stackaddr$prim47778 = alloca %struct.ScmObj*, align 8
%lst40156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46460)
store volatile %struct.ScmObj* %lst40156, %struct.ScmObj** %stackaddr$prim47778, align 8
%stackaddr$makeclosure47779 = alloca %struct.ScmObj*, align 8
%fptrToInt47780 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41605 to i64
%ae41605 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47780)
store volatile %struct.ScmObj* %ae41605, %struct.ScmObj** %stackaddr$makeclosure47779, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41605, %struct.ScmObj* %lst40156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41605, %struct.ScmObj* %_37foldr140124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41605, %struct.ScmObj* %k40451, i64 2)
%ae41606 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47781 = alloca %struct.ScmObj*, align 8
%fptrToInt47782 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41607 to i64
%ae41607 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47782)
store volatile %struct.ScmObj* %ae41607, %struct.ScmObj** %stackaddr$makeclosure47781, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41607, %struct.ScmObj* %f40157, i64 0)
%args46475$ae41605$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47783 = alloca %struct.ScmObj*, align 8
%args46475$ae41605$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41607, %struct.ScmObj* %args46475$ae41605$0)
store volatile %struct.ScmObj* %args46475$ae41605$1, %struct.ScmObj** %stackaddr$prim47783, align 8
%stackaddr$prim47784 = alloca %struct.ScmObj*, align 8
%args46475$ae41605$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41606, %struct.ScmObj* %args46475$ae41605$1)
store volatile %struct.ScmObj* %args46475$ae41605$2, %struct.ScmObj** %stackaddr$prim47784, align 8
%clofunc47785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41605)
musttail call tailcc void %clofunc47785(%struct.ScmObj* %ae41605, %struct.ScmObj* %args46475$ae41605$2)
ret void
}

define tailcc void @proc_clo$ae41605(%struct.ScmObj* %env$ae41605,%struct.ScmObj* %current_45args46462) {
%stackaddr$env-ref47786 = alloca %struct.ScmObj*, align 8
%lst40156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41605, i64 0)
store %struct.ScmObj* %lst40156, %struct.ScmObj** %stackaddr$env-ref47786
%stackaddr$env-ref47787 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41605, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47787
%stackaddr$env-ref47788 = alloca %struct.ScmObj*, align 8
%k40451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41605, i64 2)
store %struct.ScmObj* %k40451, %struct.ScmObj** %stackaddr$env-ref47788
%stackaddr$prim47789 = alloca %struct.ScmObj*, align 8
%_95k40452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46462)
store volatile %struct.ScmObj* %_95k40452, %struct.ScmObj** %stackaddr$prim47789, align 8
%stackaddr$prim47790 = alloca %struct.ScmObj*, align 8
%current_45args46463 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46462)
store volatile %struct.ScmObj* %current_45args46463, %struct.ScmObj** %stackaddr$prim47790, align 8
%stackaddr$prim47791 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46463)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim47791, align 8
%ae41639 = call %struct.ScmObj* @const_init_null()
%args46465$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47792 = alloca %struct.ScmObj*, align 8
%args46465$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40156, %struct.ScmObj* %args46465$_37foldr140124$0)
store volatile %struct.ScmObj* %args46465$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim47792, align 8
%stackaddr$prim47793 = alloca %struct.ScmObj*, align 8
%args46465$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41639, %struct.ScmObj* %args46465$_37foldr140124$1)
store volatile %struct.ScmObj* %args46465$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim47793, align 8
%stackaddr$prim47794 = alloca %struct.ScmObj*, align 8
%args46465$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40276, %struct.ScmObj* %args46465$_37foldr140124$2)
store volatile %struct.ScmObj* %args46465$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim47794, align 8
%stackaddr$prim47795 = alloca %struct.ScmObj*, align 8
%args46465$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40451, %struct.ScmObj* %args46465$_37foldr140124$3)
store volatile %struct.ScmObj* %args46465$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim47795, align 8
%clofunc47796 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc47796(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args46465$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae41607(%struct.ScmObj* %env$ae41607,%struct.ScmObj* %current_45args46466) {
%stackaddr$env-ref47797 = alloca %struct.ScmObj*, align 8
%f40157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41607, i64 0)
store %struct.ScmObj* %f40157, %struct.ScmObj** %stackaddr$env-ref47797
%stackaddr$prim47798 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46466)
store volatile %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$prim47798, align 8
%stackaddr$prim47799 = alloca %struct.ScmObj*, align 8
%current_45args46467 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46466)
store volatile %struct.ScmObj* %current_45args46467, %struct.ScmObj** %stackaddr$prim47799, align 8
%stackaddr$prim47800 = alloca %struct.ScmObj*, align 8
%v40159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46467)
store volatile %struct.ScmObj* %v40159, %struct.ScmObj** %stackaddr$prim47800, align 8
%stackaddr$prim47801 = alloca %struct.ScmObj*, align 8
%current_45args46468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46467)
store volatile %struct.ScmObj* %current_45args46468, %struct.ScmObj** %stackaddr$prim47801, align 8
%stackaddr$prim47802 = alloca %struct.ScmObj*, align 8
%r40158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46468)
store volatile %struct.ScmObj* %r40158, %struct.ScmObj** %stackaddr$prim47802, align 8
%stackaddr$makeclosure47803 = alloca %struct.ScmObj*, align 8
%fptrToInt47804 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41609 to i64
%ae41609 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47804)
store volatile %struct.ScmObj* %ae41609, %struct.ScmObj** %stackaddr$makeclosure47803, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41609, %struct.ScmObj* %k40453, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41609, %struct.ScmObj* %r40158, i64 1)
%args46474$f40157$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47805 = alloca %struct.ScmObj*, align 8
%args46474$f40157$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40159, %struct.ScmObj* %args46474$f40157$0)
store volatile %struct.ScmObj* %args46474$f40157$1, %struct.ScmObj** %stackaddr$prim47805, align 8
%stackaddr$prim47806 = alloca %struct.ScmObj*, align 8
%args46474$f40157$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41609, %struct.ScmObj* %args46474$f40157$1)
store volatile %struct.ScmObj* %args46474$f40157$2, %struct.ScmObj** %stackaddr$prim47806, align 8
%clofunc47807 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40157)
musttail call tailcc void %clofunc47807(%struct.ScmObj* %f40157, %struct.ScmObj* %args46474$f40157$2)
ret void
}

define tailcc void @proc_clo$ae41609(%struct.ScmObj* %env$ae41609,%struct.ScmObj* %current_45args46470) {
%stackaddr$env-ref47808 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41609, i64 0)
store %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$env-ref47808
%stackaddr$env-ref47809 = alloca %struct.ScmObj*, align 8
%r40158 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41609, i64 1)
store %struct.ScmObj* %r40158, %struct.ScmObj** %stackaddr$env-ref47809
%stackaddr$prim47810 = alloca %struct.ScmObj*, align 8
%_95k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46470)
store volatile %struct.ScmObj* %_95k40454, %struct.ScmObj** %stackaddr$prim47810, align 8
%stackaddr$prim47811 = alloca %struct.ScmObj*, align 8
%current_45args46471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46470)
store volatile %struct.ScmObj* %current_45args46471, %struct.ScmObj** %stackaddr$prim47811, align 8
%stackaddr$prim47812 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46471)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim47812, align 8
%stackaddr$prim47813 = alloca %struct.ScmObj*, align 8
%cpsprim40455 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40275, %struct.ScmObj* %r40158)
store volatile %struct.ScmObj* %cpsprim40455, %struct.ScmObj** %stackaddr$prim47813, align 8
%ae41614 = call %struct.ScmObj* @const_init_int(i64 0)
%args46473$k40453$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47814 = alloca %struct.ScmObj*, align 8
%args46473$k40453$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40455, %struct.ScmObj* %args46473$k40453$0)
store volatile %struct.ScmObj* %args46473$k40453$1, %struct.ScmObj** %stackaddr$prim47814, align 8
%stackaddr$prim47815 = alloca %struct.ScmObj*, align 8
%args46473$k40453$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41614, %struct.ScmObj* %args46473$k40453$1)
store volatile %struct.ScmObj* %args46473$k40453$2, %struct.ScmObj** %stackaddr$prim47815, align 8
%clofunc47816 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40453)
musttail call tailcc void %clofunc47816(%struct.ScmObj* %k40453, %struct.ScmObj* %args46473$k40453$2)
ret void
}

define tailcc void @proc_clo$ae41218(%struct.ScmObj* %env$ae41218,%struct.ScmObj* %current_45args46478) {
%stackaddr$env-ref47817 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41218, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47817
%stackaddr$env-ref47818 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41218, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47818
%stackaddr$prim47819 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46478)
store volatile %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$prim47819, align 8
%stackaddr$prim47820 = alloca %struct.ScmObj*, align 8
%current_45args46479 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46478)
store volatile %struct.ScmObj* %current_45args46479, %struct.ScmObj** %stackaddr$prim47820, align 8
%stackaddr$prim47821 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46479)
store volatile %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$prim47821, align 8
%ae41220 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47822 = alloca %struct.ScmObj*, align 8
%fptrToInt47823 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41221 to i64
%ae41221 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47823)
store volatile %struct.ScmObj* %ae41221, %struct.ScmObj** %stackaddr$makeclosure47822, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41221, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41221, %struct.ScmObj* %_37foldr40130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41221, %struct.ScmObj* %_37foldr140124, i64 2)
%args46536$k40456$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47824 = alloca %struct.ScmObj*, align 8
%args46536$k40456$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41221, %struct.ScmObj* %args46536$k40456$0)
store volatile %struct.ScmObj* %args46536$k40456$1, %struct.ScmObj** %stackaddr$prim47824, align 8
%stackaddr$prim47825 = alloca %struct.ScmObj*, align 8
%args46536$k40456$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41220, %struct.ScmObj* %args46536$k40456$1)
store volatile %struct.ScmObj* %args46536$k40456$2, %struct.ScmObj** %stackaddr$prim47825, align 8
%clofunc47826 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40456)
musttail call tailcc void %clofunc47826(%struct.ScmObj* %k40456, %struct.ScmObj* %args46536$k40456$2)
ret void
}

define tailcc void @proc_clo$ae41221(%struct.ScmObj* %env$ae41221,%struct.ScmObj* %args4013140457) {
%stackaddr$env-ref47827 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41221, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47827
%stackaddr$env-ref47828 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41221, i64 1)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref47828
%stackaddr$env-ref47829 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41221, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47829
%stackaddr$prim47830 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013140457)
store volatile %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$prim47830, align 8
%stackaddr$prim47831 = alloca %struct.ScmObj*, align 8
%args40131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013140457)
store volatile %struct.ScmObj* %args40131, %struct.ScmObj** %stackaddr$prim47831, align 8
%stackaddr$prim47832 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40131)
store volatile %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$prim47832, align 8
%stackaddr$prim47833 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40131)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim47833, align 8
%stackaddr$prim47834 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40262)
store volatile %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$prim47834, align 8
%stackaddr$prim47835 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40131)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim47835, align 8
%stackaddr$prim47836 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40263)
store volatile %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$prim47836, align 8
%stackaddr$makeclosure47837 = alloca %struct.ScmObj*, align 8
%fptrToInt47838 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41229 to i64
%ae41229 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47838)
store volatile %struct.ScmObj* %ae41229, %struct.ScmObj** %stackaddr$makeclosure47837, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41229, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41229, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41229, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41229, %struct.ScmObj* %lsts40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41229, %struct.ScmObj* %_37foldr40130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41229, %struct.ScmObj* %_37foldr140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41229, %struct.ScmObj* %k40458, i64 6)
%ae41230 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47839 = alloca %struct.ScmObj*, align 8
%fptrToInt47840 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41231 to i64
%ae41231 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47840)
store volatile %struct.ScmObj* %ae41231, %struct.ScmObj** %stackaddr$makeclosure47839, align 8
%args46535$ae41229$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47841 = alloca %struct.ScmObj*, align 8
%args46535$ae41229$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41231, %struct.ScmObj* %args46535$ae41229$0)
store volatile %struct.ScmObj* %args46535$ae41229$1, %struct.ScmObj** %stackaddr$prim47841, align 8
%stackaddr$prim47842 = alloca %struct.ScmObj*, align 8
%args46535$ae41229$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41230, %struct.ScmObj* %args46535$ae41229$1)
store volatile %struct.ScmObj* %args46535$ae41229$2, %struct.ScmObj** %stackaddr$prim47842, align 8
%clofunc47843 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41229)
musttail call tailcc void %clofunc47843(%struct.ScmObj* %ae41229, %struct.ScmObj* %args46535$ae41229$2)
ret void
}

define tailcc void @proc_clo$ae41229(%struct.ScmObj* %env$ae41229,%struct.ScmObj* %current_45args46481) {
%stackaddr$env-ref47844 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41229, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47844
%stackaddr$env-ref47845 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41229, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref47845
%stackaddr$env-ref47846 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41229, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref47846
%stackaddr$env-ref47847 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41229, i64 3)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref47847
%stackaddr$env-ref47848 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41229, i64 4)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref47848
%stackaddr$env-ref47849 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41229, i64 5)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47849
%stackaddr$env-ref47850 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41229, i64 6)
store %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$env-ref47850
%stackaddr$prim47851 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46481)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim47851, align 8
%stackaddr$prim47852 = alloca %struct.ScmObj*, align 8
%current_45args46482 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46481)
store volatile %struct.ScmObj* %current_45args46482, %struct.ScmObj** %stackaddr$prim47852, align 8
%stackaddr$prim47853 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46482)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim47853, align 8
%stackaddr$makeclosure47854 = alloca %struct.ScmObj*, align 8
%fptrToInt47855 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41261 to i64
%ae41261 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47855)
store volatile %struct.ScmObj* %ae41261, %struct.ScmObj** %stackaddr$makeclosure47854, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41261, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41261, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41261, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41261, %struct.ScmObj* %lsts40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41261, %struct.ScmObj* %_37foldr40130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41261, %struct.ScmObj* %_37foldr140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41261, %struct.ScmObj* %k40458, i64 6)
%ae41263 = call %struct.ScmObj* @const_init_false()
%args46528$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47856 = alloca %struct.ScmObj*, align 8
%args46528$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40132, %struct.ScmObj* %args46528$_37foldr140124$0)
store volatile %struct.ScmObj* %args46528$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim47856, align 8
%stackaddr$prim47857 = alloca %struct.ScmObj*, align 8
%args46528$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41263, %struct.ScmObj* %args46528$_37foldr140124$1)
store volatile %struct.ScmObj* %args46528$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim47857, align 8
%stackaddr$prim47858 = alloca %struct.ScmObj*, align 8
%args46528$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40264, %struct.ScmObj* %args46528$_37foldr140124$2)
store volatile %struct.ScmObj* %args46528$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim47858, align 8
%stackaddr$prim47859 = alloca %struct.ScmObj*, align 8
%args46528$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41261, %struct.ScmObj* %args46528$_37foldr140124$3)
store volatile %struct.ScmObj* %args46528$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim47859, align 8
%clofunc47860 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc47860(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args46528$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae41261(%struct.ScmObj* %env$ae41261,%struct.ScmObj* %current_45args46484) {
%stackaddr$env-ref47861 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41261, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47861
%stackaddr$env-ref47862 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41261, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref47862
%stackaddr$env-ref47863 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41261, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref47863
%stackaddr$env-ref47864 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41261, i64 3)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref47864
%stackaddr$env-ref47865 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41261, i64 4)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref47865
%stackaddr$env-ref47866 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41261, i64 5)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47866
%stackaddr$env-ref47867 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41261, i64 6)
store %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$env-ref47867
%stackaddr$prim47868 = alloca %struct.ScmObj*, align 8
%_95k40460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46484)
store volatile %struct.ScmObj* %_95k40460, %struct.ScmObj** %stackaddr$prim47868, align 8
%stackaddr$prim47869 = alloca %struct.ScmObj*, align 8
%current_45args46485 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46484)
store volatile %struct.ScmObj* %current_45args46485, %struct.ScmObj** %stackaddr$prim47869, align 8
%stackaddr$prim47870 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46485)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim47870, align 8
%truthy$cmp47871 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40265)
%cmp$cmp47871 = icmp eq i64 %truthy$cmp47871, 1
br i1 %cmp$cmp47871, label %truebranch$cmp47871, label %falsebranch$cmp47871
truebranch$cmp47871:
%ae41272 = call %struct.ScmObj* @const_init_int(i64 0)
%args46487$k40458$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47872 = alloca %struct.ScmObj*, align 8
%args46487$k40458$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40133, %struct.ScmObj* %args46487$k40458$0)
store volatile %struct.ScmObj* %args46487$k40458$1, %struct.ScmObj** %stackaddr$prim47872, align 8
%stackaddr$prim47873 = alloca %struct.ScmObj*, align 8
%args46487$k40458$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41272, %struct.ScmObj* %args46487$k40458$1)
store volatile %struct.ScmObj* %args46487$k40458$2, %struct.ScmObj** %stackaddr$prim47873, align 8
%clofunc47874 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40458)
musttail call tailcc void %clofunc47874(%struct.ScmObj* %k40458, %struct.ScmObj* %args46487$k40458$2)
ret void
falsebranch$cmp47871:
%stackaddr$makeclosure47875 = alloca %struct.ScmObj*, align 8
%fptrToInt47876 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41277 to i64
%ae41277 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47876)
store volatile %struct.ScmObj* %ae41277, %struct.ScmObj** %stackaddr$makeclosure47875, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %lsts40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %_37foldr40130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %_37foldr140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %k40458, i64 6)
%ae41278 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47877 = alloca %struct.ScmObj*, align 8
%fptrToInt47878 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41279 to i64
%ae41279 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47878)
store volatile %struct.ScmObj* %ae41279, %struct.ScmObj** %stackaddr$makeclosure47877, align 8
%args46527$ae41277$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47879 = alloca %struct.ScmObj*, align 8
%args46527$ae41277$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41279, %struct.ScmObj* %args46527$ae41277$0)
store volatile %struct.ScmObj* %args46527$ae41277$1, %struct.ScmObj** %stackaddr$prim47879, align 8
%stackaddr$prim47880 = alloca %struct.ScmObj*, align 8
%args46527$ae41277$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41278, %struct.ScmObj* %args46527$ae41277$1)
store volatile %struct.ScmObj* %args46527$ae41277$2, %struct.ScmObj** %stackaddr$prim47880, align 8
%clofunc47881 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41277)
musttail call tailcc void %clofunc47881(%struct.ScmObj* %ae41277, %struct.ScmObj* %args46527$ae41277$2)
ret void
}

define tailcc void @proc_clo$ae41277(%struct.ScmObj* %env$ae41277,%struct.ScmObj* %current_45args46488) {
%stackaddr$env-ref47882 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47882
%stackaddr$env-ref47883 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref47883
%stackaddr$env-ref47884 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref47884
%stackaddr$env-ref47885 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 3)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref47885
%stackaddr$env-ref47886 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 4)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref47886
%stackaddr$env-ref47887 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 5)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47887
%stackaddr$env-ref47888 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 6)
store %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$env-ref47888
%stackaddr$prim47889 = alloca %struct.ScmObj*, align 8
%_95k40461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46488)
store volatile %struct.ScmObj* %_95k40461, %struct.ScmObj** %stackaddr$prim47889, align 8
%stackaddr$prim47890 = alloca %struct.ScmObj*, align 8
%current_45args46489 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46488)
store volatile %struct.ScmObj* %current_45args46489, %struct.ScmObj** %stackaddr$prim47890, align 8
%stackaddr$prim47891 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46489)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim47891, align 8
%stackaddr$makeclosure47892 = alloca %struct.ScmObj*, align 8
%fptrToInt47893 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41298 to i64
%ae41298 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47893)
store volatile %struct.ScmObj* %ae41298, %struct.ScmObj** %stackaddr$makeclosure47892, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %lsts40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %_37foldr40130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %_37foldr140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %k40458, i64 6)
%args46522$_37map140120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47894 = alloca %struct.ScmObj*, align 8
%args46522$_37map140120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40132, %struct.ScmObj* %args46522$_37map140120$0)
store volatile %struct.ScmObj* %args46522$_37map140120$1, %struct.ScmObj** %stackaddr$prim47894, align 8
%stackaddr$prim47895 = alloca %struct.ScmObj*, align 8
%args46522$_37map140120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40266, %struct.ScmObj* %args46522$_37map140120$1)
store volatile %struct.ScmObj* %args46522$_37map140120$2, %struct.ScmObj** %stackaddr$prim47895, align 8
%stackaddr$prim47896 = alloca %struct.ScmObj*, align 8
%args46522$_37map140120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41298, %struct.ScmObj* %args46522$_37map140120$2)
store volatile %struct.ScmObj* %args46522$_37map140120$3, %struct.ScmObj** %stackaddr$prim47896, align 8
%clofunc47897 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140120)
musttail call tailcc void %clofunc47897(%struct.ScmObj* %_37map140120, %struct.ScmObj* %args46522$_37map140120$3)
ret void
}

define tailcc void @proc_clo$ae41298(%struct.ScmObj* %env$ae41298,%struct.ScmObj* %current_45args46491) {
%stackaddr$env-ref47898 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47898
%stackaddr$env-ref47899 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref47899
%stackaddr$env-ref47900 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref47900
%stackaddr$env-ref47901 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 3)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref47901
%stackaddr$env-ref47902 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 4)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref47902
%stackaddr$env-ref47903 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 5)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47903
%stackaddr$env-ref47904 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 6)
store %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$env-ref47904
%stackaddr$prim47905 = alloca %struct.ScmObj*, align 8
%_95k40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46491)
store volatile %struct.ScmObj* %_95k40462, %struct.ScmObj** %stackaddr$prim47905, align 8
%stackaddr$prim47906 = alloca %struct.ScmObj*, align 8
%current_45args46492 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46491)
store volatile %struct.ScmObj* %current_45args46492, %struct.ScmObj** %stackaddr$prim47906, align 8
%stackaddr$prim47907 = alloca %struct.ScmObj*, align 8
%lsts_4340139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46492)
store volatile %struct.ScmObj* %lsts_4340139, %struct.ScmObj** %stackaddr$prim47907, align 8
%stackaddr$makeclosure47908 = alloca %struct.ScmObj*, align 8
%fptrToInt47909 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41301 to i64
%ae41301 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt47909)
store volatile %struct.ScmObj* %ae41301, %struct.ScmObj** %stackaddr$makeclosure47908, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %lsts40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %_37foldr40130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %_37foldr140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %lsts_4340139, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %k40458, i64 7)
%ae41302 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47910 = alloca %struct.ScmObj*, align 8
%fptrToInt47911 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41303 to i64
%ae41303 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47911)
store volatile %struct.ScmObj* %ae41303, %struct.ScmObj** %stackaddr$makeclosure47910, align 8
%args46521$ae41301$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47912 = alloca %struct.ScmObj*, align 8
%args46521$ae41301$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41303, %struct.ScmObj* %args46521$ae41301$0)
store volatile %struct.ScmObj* %args46521$ae41301$1, %struct.ScmObj** %stackaddr$prim47912, align 8
%stackaddr$prim47913 = alloca %struct.ScmObj*, align 8
%args46521$ae41301$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41302, %struct.ScmObj* %args46521$ae41301$1)
store volatile %struct.ScmObj* %args46521$ae41301$2, %struct.ScmObj** %stackaddr$prim47913, align 8
%clofunc47914 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41301)
musttail call tailcc void %clofunc47914(%struct.ScmObj* %ae41301, %struct.ScmObj* %args46521$ae41301$2)
ret void
}

define tailcc void @proc_clo$ae41301(%struct.ScmObj* %env$ae41301,%struct.ScmObj* %current_45args46494) {
%stackaddr$env-ref47915 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47915
%stackaddr$env-ref47916 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref47916
%stackaddr$env-ref47917 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref47917
%stackaddr$env-ref47918 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 3)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref47918
%stackaddr$env-ref47919 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 4)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref47919
%stackaddr$env-ref47920 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 5)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47920
%stackaddr$env-ref47921 = alloca %struct.ScmObj*, align 8
%lsts_4340139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 6)
store %struct.ScmObj* %lsts_4340139, %struct.ScmObj** %stackaddr$env-ref47921
%stackaddr$env-ref47922 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 7)
store %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$env-ref47922
%stackaddr$prim47923 = alloca %struct.ScmObj*, align 8
%_95k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46494)
store volatile %struct.ScmObj* %_95k40463, %struct.ScmObj** %stackaddr$prim47923, align 8
%stackaddr$prim47924 = alloca %struct.ScmObj*, align 8
%current_45args46495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46494)
store volatile %struct.ScmObj* %current_45args46495, %struct.ScmObj** %stackaddr$prim47924, align 8
%stackaddr$prim47925 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46495)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim47925, align 8
%stackaddr$makeclosure47926 = alloca %struct.ScmObj*, align 8
%fptrToInt47927 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41322 to i64
%ae41322 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47927)
store volatile %struct.ScmObj* %ae41322, %struct.ScmObj** %stackaddr$makeclosure47926, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %f40134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %acc40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %_37foldr40130, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %_37foldr140124, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %lsts_4340139, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %k40458, i64 5)
%args46516$_37map140120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47928 = alloca %struct.ScmObj*, align 8
%args46516$_37map140120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40132, %struct.ScmObj* %args46516$_37map140120$0)
store volatile %struct.ScmObj* %args46516$_37map140120$1, %struct.ScmObj** %stackaddr$prim47928, align 8
%stackaddr$prim47929 = alloca %struct.ScmObj*, align 8
%args46516$_37map140120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40267, %struct.ScmObj* %args46516$_37map140120$1)
store volatile %struct.ScmObj* %args46516$_37map140120$2, %struct.ScmObj** %stackaddr$prim47929, align 8
%stackaddr$prim47930 = alloca %struct.ScmObj*, align 8
%args46516$_37map140120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41322, %struct.ScmObj* %args46516$_37map140120$2)
store volatile %struct.ScmObj* %args46516$_37map140120$3, %struct.ScmObj** %stackaddr$prim47930, align 8
%clofunc47931 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140120)
musttail call tailcc void %clofunc47931(%struct.ScmObj* %_37map140120, %struct.ScmObj* %args46516$_37map140120$3)
ret void
}

define tailcc void @proc_clo$ae41322(%struct.ScmObj* %env$ae41322,%struct.ScmObj* %current_45args46497) {
%stackaddr$env-ref47932 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 0)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref47932
%stackaddr$env-ref47933 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 1)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref47933
%stackaddr$env-ref47934 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 2)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref47934
%stackaddr$env-ref47935 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47935
%stackaddr$env-ref47936 = alloca %struct.ScmObj*, align 8
%lsts_4340139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 4)
store %struct.ScmObj* %lsts_4340139, %struct.ScmObj** %stackaddr$env-ref47936
%stackaddr$env-ref47937 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 5)
store %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$env-ref47937
%stackaddr$prim47938 = alloca %struct.ScmObj*, align 8
%_95k40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46497)
store volatile %struct.ScmObj* %_95k40464, %struct.ScmObj** %stackaddr$prim47938, align 8
%stackaddr$prim47939 = alloca %struct.ScmObj*, align 8
%current_45args46498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46497)
store volatile %struct.ScmObj* %current_45args46498, %struct.ScmObj** %stackaddr$prim47939, align 8
%stackaddr$prim47940 = alloca %struct.ScmObj*, align 8
%vs40137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46498)
store volatile %struct.ScmObj* %vs40137, %struct.ScmObj** %stackaddr$prim47940, align 8
%stackaddr$makeclosure47941 = alloca %struct.ScmObj*, align 8
%fptrToInt47942 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41325 to i64
%ae41325 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47942)
store volatile %struct.ScmObj* %ae41325, %struct.ScmObj** %stackaddr$makeclosure47941, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %vs40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %_37foldr40130, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %_37foldr140124, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %lsts_4340139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %k40458, i64 6)
%ae41326 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47943 = alloca %struct.ScmObj*, align 8
%fptrToInt47944 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41327 to i64
%ae41327 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47944)
store volatile %struct.ScmObj* %ae41327, %struct.ScmObj** %stackaddr$makeclosure47943, align 8
%args46515$ae41325$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47945 = alloca %struct.ScmObj*, align 8
%args46515$ae41325$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41327, %struct.ScmObj* %args46515$ae41325$0)
store volatile %struct.ScmObj* %args46515$ae41325$1, %struct.ScmObj** %stackaddr$prim47945, align 8
%stackaddr$prim47946 = alloca %struct.ScmObj*, align 8
%args46515$ae41325$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41326, %struct.ScmObj* %args46515$ae41325$1)
store volatile %struct.ScmObj* %args46515$ae41325$2, %struct.ScmObj** %stackaddr$prim47946, align 8
%clofunc47947 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41325)
musttail call tailcc void %clofunc47947(%struct.ScmObj* %ae41325, %struct.ScmObj* %args46515$ae41325$2)
ret void
}

define tailcc void @proc_clo$ae41325(%struct.ScmObj* %env$ae41325,%struct.ScmObj* %current_45args46500) {
%stackaddr$env-ref47948 = alloca %struct.ScmObj*, align 8
%vs40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 0)
store %struct.ScmObj* %vs40137, %struct.ScmObj** %stackaddr$env-ref47948
%stackaddr$env-ref47949 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref47949
%stackaddr$env-ref47950 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref47950
%stackaddr$env-ref47951 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 3)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref47951
%stackaddr$env-ref47952 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 4)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47952
%stackaddr$env-ref47953 = alloca %struct.ScmObj*, align 8
%lsts_4340139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 5)
store %struct.ScmObj* %lsts_4340139, %struct.ScmObj** %stackaddr$env-ref47953
%stackaddr$env-ref47954 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 6)
store %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$env-ref47954
%stackaddr$prim47955 = alloca %struct.ScmObj*, align 8
%_95k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46500)
store volatile %struct.ScmObj* %_95k40465, %struct.ScmObj** %stackaddr$prim47955, align 8
%stackaddr$prim47956 = alloca %struct.ScmObj*, align 8
%current_45args46501 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46500)
store volatile %struct.ScmObj* %current_45args46501, %struct.ScmObj** %stackaddr$prim47956, align 8
%stackaddr$prim47957 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46501)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim47957, align 8
%stackaddr$prim47958 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40133, %struct.ScmObj* %lsts_4340139)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim47958, align 8
%stackaddr$prim47959 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40134, %struct.ScmObj* %anf_45bind40269)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim47959, align 8
%stackaddr$makeclosure47960 = alloca %struct.ScmObj*, align 8
%fptrToInt47961 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41351 to i64
%ae41351 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47961)
store volatile %struct.ScmObj* %ae41351, %struct.ScmObj** %stackaddr$makeclosure47960, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41351, %struct.ScmObj* %anf_45bind40268, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41351, %struct.ScmObj* %_37foldr140124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41351, %struct.ScmObj* %vs40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41351, %struct.ScmObj* %f40134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41351, %struct.ScmObj* %k40458, i64 4)
%stackaddr$prim47962 = alloca %struct.ScmObj*, align 8
%cpsargs40469 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41351, %struct.ScmObj* %anf_45bind40270)
store volatile %struct.ScmObj* %cpsargs40469, %struct.ScmObj** %stackaddr$prim47962, align 8
%clofunc47963 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40130)
musttail call tailcc void %clofunc47963(%struct.ScmObj* %_37foldr40130, %struct.ScmObj* %cpsargs40469)
ret void
}

define tailcc void @proc_clo$ae41351(%struct.ScmObj* %env$ae41351,%struct.ScmObj* %current_45args46503) {
%stackaddr$env-ref47964 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41351, i64 0)
store %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$env-ref47964
%stackaddr$env-ref47965 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41351, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47965
%stackaddr$env-ref47966 = alloca %struct.ScmObj*, align 8
%vs40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41351, i64 2)
store %struct.ScmObj* %vs40137, %struct.ScmObj** %stackaddr$env-ref47966
%stackaddr$env-ref47967 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41351, i64 3)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref47967
%stackaddr$env-ref47968 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41351, i64 4)
store %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$env-ref47968
%stackaddr$prim47969 = alloca %struct.ScmObj*, align 8
%_95k40466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46503)
store volatile %struct.ScmObj* %_95k40466, %struct.ScmObj** %stackaddr$prim47969, align 8
%stackaddr$prim47970 = alloca %struct.ScmObj*, align 8
%current_45args46504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46503)
store volatile %struct.ScmObj* %current_45args46504, %struct.ScmObj** %stackaddr$prim47970, align 8
%stackaddr$prim47971 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46504)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim47971, align 8
%ae41356 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47972 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40271, %struct.ScmObj* %ae41356)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim47972, align 8
%stackaddr$makeclosure47973 = alloca %struct.ScmObj*, align 8
%fptrToInt47974 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41358 to i64
%ae41358 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47974)
store volatile %struct.ScmObj* %ae41358, %struct.ScmObj** %stackaddr$makeclosure47973, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %f40134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %k40458, i64 1)
%args46509$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47975 = alloca %struct.ScmObj*, align 8
%args46509$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40137, %struct.ScmObj* %args46509$_37foldr140124$0)
store volatile %struct.ScmObj* %args46509$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim47975, align 8
%stackaddr$prim47976 = alloca %struct.ScmObj*, align 8
%args46509$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40272, %struct.ScmObj* %args46509$_37foldr140124$1)
store volatile %struct.ScmObj* %args46509$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim47976, align 8
%stackaddr$prim47977 = alloca %struct.ScmObj*, align 8
%args46509$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40268, %struct.ScmObj* %args46509$_37foldr140124$2)
store volatile %struct.ScmObj* %args46509$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim47977, align 8
%stackaddr$prim47978 = alloca %struct.ScmObj*, align 8
%args46509$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41358, %struct.ScmObj* %args46509$_37foldr140124$3)
store volatile %struct.ScmObj* %args46509$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim47978, align 8
%clofunc47979 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc47979(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args46509$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae41358(%struct.ScmObj* %env$ae41358,%struct.ScmObj* %current_45args46506) {
%stackaddr$env-ref47980 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 0)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref47980
%stackaddr$env-ref47981 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 1)
store %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$env-ref47981
%stackaddr$prim47982 = alloca %struct.ScmObj*, align 8
%_95k40467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46506)
store volatile %struct.ScmObj* %_95k40467, %struct.ScmObj** %stackaddr$prim47982, align 8
%stackaddr$prim47983 = alloca %struct.ScmObj*, align 8
%current_45args46507 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46506)
store volatile %struct.ScmObj* %current_45args46507, %struct.ScmObj** %stackaddr$prim47983, align 8
%stackaddr$prim47984 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46507)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim47984, align 8
%stackaddr$prim47985 = alloca %struct.ScmObj*, align 8
%cpsargs40468 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40458, %struct.ScmObj* %anf_45bind40273)
store volatile %struct.ScmObj* %cpsargs40468, %struct.ScmObj** %stackaddr$prim47985, align 8
%clofunc47986 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40134)
musttail call tailcc void %clofunc47986(%struct.ScmObj* %f40134, %struct.ScmObj* %cpsargs40468)
ret void
}

define tailcc void @proc_clo$ae41327(%struct.ScmObj* %env$ae41327,%struct.ScmObj* %current_45args46510) {
%stackaddr$prim47987 = alloca %struct.ScmObj*, align 8
%k40470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46510)
store volatile %struct.ScmObj* %k40470, %struct.ScmObj** %stackaddr$prim47987, align 8
%stackaddr$prim47988 = alloca %struct.ScmObj*, align 8
%current_45args46511 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46510)
store volatile %struct.ScmObj* %current_45args46511, %struct.ScmObj** %stackaddr$prim47988, align 8
%stackaddr$prim47989 = alloca %struct.ScmObj*, align 8
%a40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46511)
store volatile %struct.ScmObj* %a40142, %struct.ScmObj** %stackaddr$prim47989, align 8
%stackaddr$prim47990 = alloca %struct.ScmObj*, align 8
%current_45args46512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46511)
store volatile %struct.ScmObj* %current_45args46512, %struct.ScmObj** %stackaddr$prim47990, align 8
%stackaddr$prim47991 = alloca %struct.ScmObj*, align 8
%b40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46512)
store volatile %struct.ScmObj* %b40141, %struct.ScmObj** %stackaddr$prim47991, align 8
%stackaddr$prim47992 = alloca %struct.ScmObj*, align 8
%cpsprim40471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40142, %struct.ScmObj* %b40141)
store volatile %struct.ScmObj* %cpsprim40471, %struct.ScmObj** %stackaddr$prim47992, align 8
%ae41331 = call %struct.ScmObj* @const_init_int(i64 0)
%args46514$k40470$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47993 = alloca %struct.ScmObj*, align 8
%args46514$k40470$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40471, %struct.ScmObj* %args46514$k40470$0)
store volatile %struct.ScmObj* %args46514$k40470$1, %struct.ScmObj** %stackaddr$prim47993, align 8
%stackaddr$prim47994 = alloca %struct.ScmObj*, align 8
%args46514$k40470$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41331, %struct.ScmObj* %args46514$k40470$1)
store volatile %struct.ScmObj* %args46514$k40470$2, %struct.ScmObj** %stackaddr$prim47994, align 8
%clofunc47995 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40470)
musttail call tailcc void %clofunc47995(%struct.ScmObj* %k40470, %struct.ScmObj* %args46514$k40470$2)
ret void
}

define tailcc void @proc_clo$ae41303(%struct.ScmObj* %env$ae41303,%struct.ScmObj* %current_45args46517) {
%stackaddr$prim47996 = alloca %struct.ScmObj*, align 8
%k40472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46517)
store volatile %struct.ScmObj* %k40472, %struct.ScmObj** %stackaddr$prim47996, align 8
%stackaddr$prim47997 = alloca %struct.ScmObj*, align 8
%current_45args46518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46517)
store volatile %struct.ScmObj* %current_45args46518, %struct.ScmObj** %stackaddr$prim47997, align 8
%stackaddr$prim47998 = alloca %struct.ScmObj*, align 8
%x40138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46518)
store volatile %struct.ScmObj* %x40138, %struct.ScmObj** %stackaddr$prim47998, align 8
%stackaddr$prim47999 = alloca %struct.ScmObj*, align 8
%cpsprim40473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40138)
store volatile %struct.ScmObj* %cpsprim40473, %struct.ScmObj** %stackaddr$prim47999, align 8
%ae41306 = call %struct.ScmObj* @const_init_int(i64 0)
%args46520$k40472$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48000 = alloca %struct.ScmObj*, align 8
%args46520$k40472$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40473, %struct.ScmObj* %args46520$k40472$0)
store volatile %struct.ScmObj* %args46520$k40472$1, %struct.ScmObj** %stackaddr$prim48000, align 8
%stackaddr$prim48001 = alloca %struct.ScmObj*, align 8
%args46520$k40472$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41306, %struct.ScmObj* %args46520$k40472$1)
store volatile %struct.ScmObj* %args46520$k40472$2, %struct.ScmObj** %stackaddr$prim48001, align 8
%clofunc48002 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40472)
musttail call tailcc void %clofunc48002(%struct.ScmObj* %k40472, %struct.ScmObj* %args46520$k40472$2)
ret void
}

define tailcc void @proc_clo$ae41279(%struct.ScmObj* %env$ae41279,%struct.ScmObj* %current_45args46523) {
%stackaddr$prim48003 = alloca %struct.ScmObj*, align 8
%k40474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46523)
store volatile %struct.ScmObj* %k40474, %struct.ScmObj** %stackaddr$prim48003, align 8
%stackaddr$prim48004 = alloca %struct.ScmObj*, align 8
%current_45args46524 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46523)
store volatile %struct.ScmObj* %current_45args46524, %struct.ScmObj** %stackaddr$prim48004, align 8
%stackaddr$prim48005 = alloca %struct.ScmObj*, align 8
%x40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46524)
store volatile %struct.ScmObj* %x40140, %struct.ScmObj** %stackaddr$prim48005, align 8
%stackaddr$prim48006 = alloca %struct.ScmObj*, align 8
%cpsprim40475 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40140)
store volatile %struct.ScmObj* %cpsprim40475, %struct.ScmObj** %stackaddr$prim48006, align 8
%ae41282 = call %struct.ScmObj* @const_init_int(i64 0)
%args46526$k40474$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48007 = alloca %struct.ScmObj*, align 8
%args46526$k40474$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40475, %struct.ScmObj* %args46526$k40474$0)
store volatile %struct.ScmObj* %args46526$k40474$1, %struct.ScmObj** %stackaddr$prim48007, align 8
%stackaddr$prim48008 = alloca %struct.ScmObj*, align 8
%args46526$k40474$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41282, %struct.ScmObj* %args46526$k40474$1)
store volatile %struct.ScmObj* %args46526$k40474$2, %struct.ScmObj** %stackaddr$prim48008, align 8
%clofunc48009 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40474)
musttail call tailcc void %clofunc48009(%struct.ScmObj* %k40474, %struct.ScmObj* %args46526$k40474$2)
ret void
}

define tailcc void @proc_clo$ae41231(%struct.ScmObj* %env$ae41231,%struct.ScmObj* %current_45args46529) {
%stackaddr$prim48010 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46529)
store volatile %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$prim48010, align 8
%stackaddr$prim48011 = alloca %struct.ScmObj*, align 8
%current_45args46530 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46529)
store volatile %struct.ScmObj* %current_45args46530, %struct.ScmObj** %stackaddr$prim48011, align 8
%stackaddr$prim48012 = alloca %struct.ScmObj*, align 8
%lst40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46530)
store volatile %struct.ScmObj* %lst40136, %struct.ScmObj** %stackaddr$prim48012, align 8
%stackaddr$prim48013 = alloca %struct.ScmObj*, align 8
%current_45args46531 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46530)
store volatile %struct.ScmObj* %current_45args46531, %struct.ScmObj** %stackaddr$prim48013, align 8
%stackaddr$prim48014 = alloca %struct.ScmObj*, align 8
%b40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46531)
store volatile %struct.ScmObj* %b40135, %struct.ScmObj** %stackaddr$prim48014, align 8
%truthy$cmp48015 = call i64 @is_truthy_value(%struct.ScmObj* %b40135)
%cmp$cmp48015 = icmp eq i64 %truthy$cmp48015, 1
br i1 %cmp$cmp48015, label %truebranch$cmp48015, label %falsebranch$cmp48015
truebranch$cmp48015:
%ae41234 = call %struct.ScmObj* @const_init_int(i64 0)
%args46533$k40476$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48016 = alloca %struct.ScmObj*, align 8
%args46533$k40476$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40135, %struct.ScmObj* %args46533$k40476$0)
store volatile %struct.ScmObj* %args46533$k40476$1, %struct.ScmObj** %stackaddr$prim48016, align 8
%stackaddr$prim48017 = alloca %struct.ScmObj*, align 8
%args46533$k40476$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41234, %struct.ScmObj* %args46533$k40476$1)
store volatile %struct.ScmObj* %args46533$k40476$2, %struct.ScmObj** %stackaddr$prim48017, align 8
%clofunc48018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40476)
musttail call tailcc void %clofunc48018(%struct.ScmObj* %k40476, %struct.ScmObj* %args46533$k40476$2)
ret void
falsebranch$cmp48015:
%stackaddr$prim48019 = alloca %struct.ScmObj*, align 8
%cpsprim40477 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40136)
store volatile %struct.ScmObj* %cpsprim40477, %struct.ScmObj** %stackaddr$prim48019, align 8
%ae41241 = call %struct.ScmObj* @const_init_int(i64 0)
%args46534$k40476$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48020 = alloca %struct.ScmObj*, align 8
%args46534$k40476$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40477, %struct.ScmObj* %args46534$k40476$0)
store volatile %struct.ScmObj* %args46534$k40476$1, %struct.ScmObj** %stackaddr$prim48020, align 8
%stackaddr$prim48021 = alloca %struct.ScmObj*, align 8
%args46534$k40476$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41241, %struct.ScmObj* %args46534$k40476$1)
store volatile %struct.ScmObj* %args46534$k40476$2, %struct.ScmObj** %stackaddr$prim48021, align 8
%clofunc48022 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40476)
musttail call tailcc void %clofunc48022(%struct.ScmObj* %k40476, %struct.ScmObj* %args46534$k40476$2)
ret void
}

define tailcc void @proc_clo$ae41188(%struct.ScmObj* %env$ae41188,%struct.ScmObj* %current_45args46538) {
%stackaddr$env-ref48023 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41188, i64 0)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref48023
%stackaddr$env-ref48024 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41188, i64 1)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref48024
%stackaddr$prim48025 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46538)
store volatile %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$prim48025, align 8
%stackaddr$prim48026 = alloca %struct.ScmObj*, align 8
%current_45args46539 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46538)
store volatile %struct.ScmObj* %current_45args46539, %struct.ScmObj** %stackaddr$prim48026, align 8
%stackaddr$prim48027 = alloca %struct.ScmObj*, align 8
%lst40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46539)
store volatile %struct.ScmObj* %lst40145, %struct.ScmObj** %stackaddr$prim48027, align 8
%stackaddr$prim48028 = alloca %struct.ScmObj*, align 8
%current_45args46540 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46539)
store volatile %struct.ScmObj* %current_45args46540, %struct.ScmObj** %stackaddr$prim48028, align 8
%stackaddr$prim48029 = alloca %struct.ScmObj*, align 8
%n40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46540)
store volatile %struct.ScmObj* %n40144, %struct.ScmObj** %stackaddr$prim48029, align 8
%stackaddr$makeclosure48030 = alloca %struct.ScmObj*, align 8
%fptrToInt48031 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41190 to i64
%ae41190 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48031)
store volatile %struct.ScmObj* %ae41190, %struct.ScmObj** %stackaddr$makeclosure48030, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41190, %struct.ScmObj* %_37take40116, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41190, %struct.ScmObj* %lst40145, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41190, %struct.ScmObj* %n40144, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41190, %struct.ScmObj* %k40478, i64 3)
%args46546$_37length40113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48032 = alloca %struct.ScmObj*, align 8
%args46546$_37length40113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40145, %struct.ScmObj* %args46546$_37length40113$0)
store volatile %struct.ScmObj* %args46546$_37length40113$1, %struct.ScmObj** %stackaddr$prim48032, align 8
%stackaddr$prim48033 = alloca %struct.ScmObj*, align 8
%args46546$_37length40113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41190, %struct.ScmObj* %args46546$_37length40113$1)
store volatile %struct.ScmObj* %args46546$_37length40113$2, %struct.ScmObj** %stackaddr$prim48033, align 8
%clofunc48034 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40113)
musttail call tailcc void %clofunc48034(%struct.ScmObj* %_37length40113, %struct.ScmObj* %args46546$_37length40113$2)
ret void
}

define tailcc void @proc_clo$ae41190(%struct.ScmObj* %env$ae41190,%struct.ScmObj* %current_45args46542) {
%stackaddr$env-ref48035 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41190, i64 0)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref48035
%stackaddr$env-ref48036 = alloca %struct.ScmObj*, align 8
%lst40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41190, i64 1)
store %struct.ScmObj* %lst40145, %struct.ScmObj** %stackaddr$env-ref48036
%stackaddr$env-ref48037 = alloca %struct.ScmObj*, align 8
%n40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41190, i64 2)
store %struct.ScmObj* %n40144, %struct.ScmObj** %stackaddr$env-ref48037
%stackaddr$env-ref48038 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41190, i64 3)
store %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$env-ref48038
%stackaddr$prim48039 = alloca %struct.ScmObj*, align 8
%_95k40479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46542)
store volatile %struct.ScmObj* %_95k40479, %struct.ScmObj** %stackaddr$prim48039, align 8
%stackaddr$prim48040 = alloca %struct.ScmObj*, align 8
%current_45args46543 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46542)
store volatile %struct.ScmObj* %current_45args46543, %struct.ScmObj** %stackaddr$prim48040, align 8
%stackaddr$prim48041 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46543)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim48041, align 8
%stackaddr$prim48042 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40260, %struct.ScmObj* %n40144)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim48042, align 8
%args46545$_37take40116$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48043 = alloca %struct.ScmObj*, align 8
%args46545$_37take40116$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40261, %struct.ScmObj* %args46545$_37take40116$0)
store volatile %struct.ScmObj* %args46545$_37take40116$1, %struct.ScmObj** %stackaddr$prim48043, align 8
%stackaddr$prim48044 = alloca %struct.ScmObj*, align 8
%args46545$_37take40116$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40145, %struct.ScmObj* %args46545$_37take40116$1)
store volatile %struct.ScmObj* %args46545$_37take40116$2, %struct.ScmObj** %stackaddr$prim48044, align 8
%stackaddr$prim48045 = alloca %struct.ScmObj*, align 8
%args46545$_37take40116$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40478, %struct.ScmObj* %args46545$_37take40116$2)
store volatile %struct.ScmObj* %args46545$_37take40116$3, %struct.ScmObj** %stackaddr$prim48045, align 8
%clofunc48046 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40116)
musttail call tailcc void %clofunc48046(%struct.ScmObj* %_37take40116, %struct.ScmObj* %args46545$_37take40116$3)
ret void
}

define tailcc void @proc_clo$ae41134(%struct.ScmObj* %env$ae41134,%struct.ScmObj* %current_45args46548) {
%stackaddr$env-ref48047 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41134, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48047
%stackaddr$prim48048 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46548)
store volatile %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$prim48048, align 8
%stackaddr$prim48049 = alloca %struct.ScmObj*, align 8
%current_45args46549 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46548)
store volatile %struct.ScmObj* %current_45args46549, %struct.ScmObj** %stackaddr$prim48049, align 8
%stackaddr$prim48050 = alloca %struct.ScmObj*, align 8
%lst40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46549)
store volatile %struct.ScmObj* %lst40147, %struct.ScmObj** %stackaddr$prim48050, align 8
%stackaddr$makeclosure48051 = alloca %struct.ScmObj*, align 8
%fptrToInt48052 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41135 to i64
%ae41135 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48052)
store volatile %struct.ScmObj* %ae41135, %struct.ScmObj** %stackaddr$makeclosure48051, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41135, %struct.ScmObj* %lst40147, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41135, %struct.ScmObj* %k40480, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41135, %struct.ScmObj* %_37foldl140108, i64 2)
%ae41136 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48053 = alloca %struct.ScmObj*, align 8
%fptrToInt48054 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41137 to i64
%ae41137 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48054)
store volatile %struct.ScmObj* %ae41137, %struct.ScmObj** %stackaddr$makeclosure48053, align 8
%args46560$ae41135$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48055 = alloca %struct.ScmObj*, align 8
%args46560$ae41135$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41137, %struct.ScmObj* %args46560$ae41135$0)
store volatile %struct.ScmObj* %args46560$ae41135$1, %struct.ScmObj** %stackaddr$prim48055, align 8
%stackaddr$prim48056 = alloca %struct.ScmObj*, align 8
%args46560$ae41135$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41136, %struct.ScmObj* %args46560$ae41135$1)
store volatile %struct.ScmObj* %args46560$ae41135$2, %struct.ScmObj** %stackaddr$prim48056, align 8
%clofunc48057 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41135)
musttail call tailcc void %clofunc48057(%struct.ScmObj* %ae41135, %struct.ScmObj* %args46560$ae41135$2)
ret void
}

define tailcc void @proc_clo$ae41135(%struct.ScmObj* %env$ae41135,%struct.ScmObj* %current_45args46551) {
%stackaddr$env-ref48058 = alloca %struct.ScmObj*, align 8
%lst40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41135, i64 0)
store %struct.ScmObj* %lst40147, %struct.ScmObj** %stackaddr$env-ref48058
%stackaddr$env-ref48059 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41135, i64 1)
store %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$env-ref48059
%stackaddr$env-ref48060 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41135, i64 2)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48060
%stackaddr$prim48061 = alloca %struct.ScmObj*, align 8
%_95k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46551)
store volatile %struct.ScmObj* %_95k40481, %struct.ScmObj** %stackaddr$prim48061, align 8
%stackaddr$prim48062 = alloca %struct.ScmObj*, align 8
%current_45args46552 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46551)
store volatile %struct.ScmObj* %current_45args46552, %struct.ScmObj** %stackaddr$prim48062, align 8
%stackaddr$prim48063 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46552)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim48063, align 8
%ae41156 = call %struct.ScmObj* @const_init_null()
%args46554$_37foldl140108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48064 = alloca %struct.ScmObj*, align 8
%args46554$_37foldl140108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40147, %struct.ScmObj* %args46554$_37foldl140108$0)
store volatile %struct.ScmObj* %args46554$_37foldl140108$1, %struct.ScmObj** %stackaddr$prim48064, align 8
%stackaddr$prim48065 = alloca %struct.ScmObj*, align 8
%args46554$_37foldl140108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41156, %struct.ScmObj* %args46554$_37foldl140108$1)
store volatile %struct.ScmObj* %args46554$_37foldl140108$2, %struct.ScmObj** %stackaddr$prim48065, align 8
%stackaddr$prim48066 = alloca %struct.ScmObj*, align 8
%args46554$_37foldl140108$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40259, %struct.ScmObj* %args46554$_37foldl140108$2)
store volatile %struct.ScmObj* %args46554$_37foldl140108$3, %struct.ScmObj** %stackaddr$prim48066, align 8
%stackaddr$prim48067 = alloca %struct.ScmObj*, align 8
%args46554$_37foldl140108$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40480, %struct.ScmObj* %args46554$_37foldl140108$3)
store volatile %struct.ScmObj* %args46554$_37foldl140108$4, %struct.ScmObj** %stackaddr$prim48067, align 8
%clofunc48068 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140108)
musttail call tailcc void %clofunc48068(%struct.ScmObj* %_37foldl140108, %struct.ScmObj* %args46554$_37foldl140108$4)
ret void
}

define tailcc void @proc_clo$ae41137(%struct.ScmObj* %env$ae41137,%struct.ScmObj* %current_45args46555) {
%stackaddr$prim48069 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46555)
store volatile %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$prim48069, align 8
%stackaddr$prim48070 = alloca %struct.ScmObj*, align 8
%current_45args46556 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46555)
store volatile %struct.ScmObj* %current_45args46556, %struct.ScmObj** %stackaddr$prim48070, align 8
%stackaddr$prim48071 = alloca %struct.ScmObj*, align 8
%x40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46556)
store volatile %struct.ScmObj* %x40149, %struct.ScmObj** %stackaddr$prim48071, align 8
%stackaddr$prim48072 = alloca %struct.ScmObj*, align 8
%current_45args46557 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46556)
store volatile %struct.ScmObj* %current_45args46557, %struct.ScmObj** %stackaddr$prim48072, align 8
%stackaddr$prim48073 = alloca %struct.ScmObj*, align 8
%y40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46557)
store volatile %struct.ScmObj* %y40148, %struct.ScmObj** %stackaddr$prim48073, align 8
%ae41139 = call %struct.ScmObj* @const_init_int(i64 0)
%args46559$k40482$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48074 = alloca %struct.ScmObj*, align 8
%args46559$k40482$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40149, %struct.ScmObj* %args46559$k40482$0)
store volatile %struct.ScmObj* %args46559$k40482$1, %struct.ScmObj** %stackaddr$prim48074, align 8
%stackaddr$prim48075 = alloca %struct.ScmObj*, align 8
%args46559$k40482$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41139, %struct.ScmObj* %args46559$k40482$1)
store volatile %struct.ScmObj* %args46559$k40482$2, %struct.ScmObj** %stackaddr$prim48075, align 8
%clofunc48076 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40482)
musttail call tailcc void %clofunc48076(%struct.ScmObj* %k40482, %struct.ScmObj* %args46559$k40482$2)
ret void
}

define tailcc void @proc_clo$ae41055(%struct.ScmObj* %env$ae41055,%struct.ScmObj* %current_45args46563) {
%stackaddr$prim48077 = alloca %struct.ScmObj*, align 8
%k40483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46563)
store volatile %struct.ScmObj* %k40483, %struct.ScmObj** %stackaddr$prim48077, align 8
%stackaddr$prim48078 = alloca %struct.ScmObj*, align 8
%current_45args46564 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46563)
store volatile %struct.ScmObj* %current_45args46564, %struct.ScmObj** %stackaddr$prim48078, align 8
%stackaddr$prim48079 = alloca %struct.ScmObj*, align 8
%_37foldl140109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46564)
store volatile %struct.ScmObj* %_37foldl140109, %struct.ScmObj** %stackaddr$prim48079, align 8
%ae41057 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48080 = alloca %struct.ScmObj*, align 8
%fptrToInt48081 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41058 to i64
%ae41058 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48081)
store volatile %struct.ScmObj* %ae41058, %struct.ScmObj** %stackaddr$makeclosure48080, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41058, %struct.ScmObj* %_37foldl140109, i64 0)
%args46577$k40483$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48082 = alloca %struct.ScmObj*, align 8
%args46577$k40483$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41058, %struct.ScmObj* %args46577$k40483$0)
store volatile %struct.ScmObj* %args46577$k40483$1, %struct.ScmObj** %stackaddr$prim48082, align 8
%stackaddr$prim48083 = alloca %struct.ScmObj*, align 8
%args46577$k40483$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41057, %struct.ScmObj* %args46577$k40483$1)
store volatile %struct.ScmObj* %args46577$k40483$2, %struct.ScmObj** %stackaddr$prim48083, align 8
%clofunc48084 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40483)
musttail call tailcc void %clofunc48084(%struct.ScmObj* %k40483, %struct.ScmObj* %args46577$k40483$2)
ret void
}

define tailcc void @proc_clo$ae41058(%struct.ScmObj* %env$ae41058,%struct.ScmObj* %current_45args46566) {
%stackaddr$env-ref48085 = alloca %struct.ScmObj*, align 8
%_37foldl140109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41058, i64 0)
store %struct.ScmObj* %_37foldl140109, %struct.ScmObj** %stackaddr$env-ref48085
%stackaddr$prim48086 = alloca %struct.ScmObj*, align 8
%k40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46566)
store volatile %struct.ScmObj* %k40484, %struct.ScmObj** %stackaddr$prim48086, align 8
%stackaddr$prim48087 = alloca %struct.ScmObj*, align 8
%current_45args46567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46566)
store volatile %struct.ScmObj* %current_45args46567, %struct.ScmObj** %stackaddr$prim48087, align 8
%stackaddr$prim48088 = alloca %struct.ScmObj*, align 8
%f40112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46567)
store volatile %struct.ScmObj* %f40112, %struct.ScmObj** %stackaddr$prim48088, align 8
%stackaddr$prim48089 = alloca %struct.ScmObj*, align 8
%current_45args46568 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46567)
store volatile %struct.ScmObj* %current_45args46568, %struct.ScmObj** %stackaddr$prim48089, align 8
%stackaddr$prim48090 = alloca %struct.ScmObj*, align 8
%acc40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46568)
store volatile %struct.ScmObj* %acc40111, %struct.ScmObj** %stackaddr$prim48090, align 8
%stackaddr$prim48091 = alloca %struct.ScmObj*, align 8
%current_45args46569 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46568)
store volatile %struct.ScmObj* %current_45args46569, %struct.ScmObj** %stackaddr$prim48091, align 8
%stackaddr$prim48092 = alloca %struct.ScmObj*, align 8
%lst40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46569)
store volatile %struct.ScmObj* %lst40110, %struct.ScmObj** %stackaddr$prim48092, align 8
%stackaddr$prim48093 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40110)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim48093, align 8
%truthy$cmp48094 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40254)
%cmp$cmp48094 = icmp eq i64 %truthy$cmp48094, 1
br i1 %cmp$cmp48094, label %truebranch$cmp48094, label %falsebranch$cmp48094
truebranch$cmp48094:
%ae41062 = call %struct.ScmObj* @const_init_int(i64 0)
%args46571$k40484$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48095 = alloca %struct.ScmObj*, align 8
%args46571$k40484$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40111, %struct.ScmObj* %args46571$k40484$0)
store volatile %struct.ScmObj* %args46571$k40484$1, %struct.ScmObj** %stackaddr$prim48095, align 8
%stackaddr$prim48096 = alloca %struct.ScmObj*, align 8
%args46571$k40484$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41062, %struct.ScmObj* %args46571$k40484$1)
store volatile %struct.ScmObj* %args46571$k40484$2, %struct.ScmObj** %stackaddr$prim48096, align 8
%clofunc48097 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40484)
musttail call tailcc void %clofunc48097(%struct.ScmObj* %k40484, %struct.ScmObj* %args46571$k40484$2)
ret void
falsebranch$cmp48094:
%stackaddr$prim48098 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40110)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim48098, align 8
%stackaddr$makeclosure48099 = alloca %struct.ScmObj*, align 8
%fptrToInt48100 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41069 to i64
%ae41069 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48100)
store volatile %struct.ScmObj* %ae41069, %struct.ScmObj** %stackaddr$makeclosure48099, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41069, %struct.ScmObj* %k40484, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41069, %struct.ScmObj* %f40112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41069, %struct.ScmObj* %lst40110, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41069, %struct.ScmObj* %_37foldl140109, i64 3)
%args46576$f40112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48101 = alloca %struct.ScmObj*, align 8
%args46576$f40112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40111, %struct.ScmObj* %args46576$f40112$0)
store volatile %struct.ScmObj* %args46576$f40112$1, %struct.ScmObj** %stackaddr$prim48101, align 8
%stackaddr$prim48102 = alloca %struct.ScmObj*, align 8
%args46576$f40112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40255, %struct.ScmObj* %args46576$f40112$1)
store volatile %struct.ScmObj* %args46576$f40112$2, %struct.ScmObj** %stackaddr$prim48102, align 8
%stackaddr$prim48103 = alloca %struct.ScmObj*, align 8
%args46576$f40112$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41069, %struct.ScmObj* %args46576$f40112$2)
store volatile %struct.ScmObj* %args46576$f40112$3, %struct.ScmObj** %stackaddr$prim48103, align 8
%clofunc48104 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40112)
musttail call tailcc void %clofunc48104(%struct.ScmObj* %f40112, %struct.ScmObj* %args46576$f40112$3)
ret void
}

define tailcc void @proc_clo$ae41069(%struct.ScmObj* %env$ae41069,%struct.ScmObj* %current_45args46572) {
%stackaddr$env-ref48105 = alloca %struct.ScmObj*, align 8
%k40484 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41069, i64 0)
store %struct.ScmObj* %k40484, %struct.ScmObj** %stackaddr$env-ref48105
%stackaddr$env-ref48106 = alloca %struct.ScmObj*, align 8
%f40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41069, i64 1)
store %struct.ScmObj* %f40112, %struct.ScmObj** %stackaddr$env-ref48106
%stackaddr$env-ref48107 = alloca %struct.ScmObj*, align 8
%lst40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41069, i64 2)
store %struct.ScmObj* %lst40110, %struct.ScmObj** %stackaddr$env-ref48107
%stackaddr$env-ref48108 = alloca %struct.ScmObj*, align 8
%_37foldl140109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41069, i64 3)
store %struct.ScmObj* %_37foldl140109, %struct.ScmObj** %stackaddr$env-ref48108
%stackaddr$prim48109 = alloca %struct.ScmObj*, align 8
%_95k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46572)
store volatile %struct.ScmObj* %_95k40485, %struct.ScmObj** %stackaddr$prim48109, align 8
%stackaddr$prim48110 = alloca %struct.ScmObj*, align 8
%current_45args46573 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46572)
store volatile %struct.ScmObj* %current_45args46573, %struct.ScmObj** %stackaddr$prim48110, align 8
%stackaddr$prim48111 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46573)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim48111, align 8
%stackaddr$prim48112 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40110)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim48112, align 8
%args46575$_37foldl140109$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48113 = alloca %struct.ScmObj*, align 8
%args46575$_37foldl140109$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40257, %struct.ScmObj* %args46575$_37foldl140109$0)
store volatile %struct.ScmObj* %args46575$_37foldl140109$1, %struct.ScmObj** %stackaddr$prim48113, align 8
%stackaddr$prim48114 = alloca %struct.ScmObj*, align 8
%args46575$_37foldl140109$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40256, %struct.ScmObj* %args46575$_37foldl140109$1)
store volatile %struct.ScmObj* %args46575$_37foldl140109$2, %struct.ScmObj** %stackaddr$prim48114, align 8
%stackaddr$prim48115 = alloca %struct.ScmObj*, align 8
%args46575$_37foldl140109$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40112, %struct.ScmObj* %args46575$_37foldl140109$2)
store volatile %struct.ScmObj* %args46575$_37foldl140109$3, %struct.ScmObj** %stackaddr$prim48115, align 8
%stackaddr$prim48116 = alloca %struct.ScmObj*, align 8
%args46575$_37foldl140109$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40484, %struct.ScmObj* %args46575$_37foldl140109$3)
store volatile %struct.ScmObj* %args46575$_37foldl140109$4, %struct.ScmObj** %stackaddr$prim48116, align 8
%clofunc48117 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140109)
musttail call tailcc void %clofunc48117(%struct.ScmObj* %_37foldl140109, %struct.ScmObj* %args46575$_37foldl140109$4)
ret void
}

define tailcc void @proc_clo$ae40972(%struct.ScmObj* %env$ae40972,%struct.ScmObj* %current_45args46580) {
%stackaddr$prim48118 = alloca %struct.ScmObj*, align 8
%k40486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46580)
store volatile %struct.ScmObj* %k40486, %struct.ScmObj** %stackaddr$prim48118, align 8
%stackaddr$prim48119 = alloca %struct.ScmObj*, align 8
%current_45args46581 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46580)
store volatile %struct.ScmObj* %current_45args46581, %struct.ScmObj** %stackaddr$prim48119, align 8
%stackaddr$prim48120 = alloca %struct.ScmObj*, align 8
%_37length40114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46581)
store volatile %struct.ScmObj* %_37length40114, %struct.ScmObj** %stackaddr$prim48120, align 8
%ae40974 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48121 = alloca %struct.ScmObj*, align 8
%fptrToInt48122 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40975 to i64
%ae40975 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48122)
store volatile %struct.ScmObj* %ae40975, %struct.ScmObj** %stackaddr$makeclosure48121, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40975, %struct.ScmObj* %_37length40114, i64 0)
%args46592$k40486$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48123 = alloca %struct.ScmObj*, align 8
%args46592$k40486$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40975, %struct.ScmObj* %args46592$k40486$0)
store volatile %struct.ScmObj* %args46592$k40486$1, %struct.ScmObj** %stackaddr$prim48123, align 8
%stackaddr$prim48124 = alloca %struct.ScmObj*, align 8
%args46592$k40486$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40974, %struct.ScmObj* %args46592$k40486$1)
store volatile %struct.ScmObj* %args46592$k40486$2, %struct.ScmObj** %stackaddr$prim48124, align 8
%clofunc48125 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40486)
musttail call tailcc void %clofunc48125(%struct.ScmObj* %k40486, %struct.ScmObj* %args46592$k40486$2)
ret void
}

define tailcc void @proc_clo$ae40975(%struct.ScmObj* %env$ae40975,%struct.ScmObj* %current_45args46583) {
%stackaddr$env-ref48126 = alloca %struct.ScmObj*, align 8
%_37length40114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40975, i64 0)
store %struct.ScmObj* %_37length40114, %struct.ScmObj** %stackaddr$env-ref48126
%stackaddr$prim48127 = alloca %struct.ScmObj*, align 8
%k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46583)
store volatile %struct.ScmObj* %k40487, %struct.ScmObj** %stackaddr$prim48127, align 8
%stackaddr$prim48128 = alloca %struct.ScmObj*, align 8
%current_45args46584 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46583)
store volatile %struct.ScmObj* %current_45args46584, %struct.ScmObj** %stackaddr$prim48128, align 8
%stackaddr$prim48129 = alloca %struct.ScmObj*, align 8
%lst40115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46584)
store volatile %struct.ScmObj* %lst40115, %struct.ScmObj** %stackaddr$prim48129, align 8
%stackaddr$prim48130 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40115)
store volatile %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$prim48130, align 8
%truthy$cmp48131 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40250)
%cmp$cmp48131 = icmp eq i64 %truthy$cmp48131, 1
br i1 %cmp$cmp48131, label %truebranch$cmp48131, label %falsebranch$cmp48131
truebranch$cmp48131:
%ae40979 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40980 = call %struct.ScmObj* @const_init_int(i64 0)
%args46586$k40487$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48132 = alloca %struct.ScmObj*, align 8
%args46586$k40487$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40980, %struct.ScmObj* %args46586$k40487$0)
store volatile %struct.ScmObj* %args46586$k40487$1, %struct.ScmObj** %stackaddr$prim48132, align 8
%stackaddr$prim48133 = alloca %struct.ScmObj*, align 8
%args46586$k40487$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40979, %struct.ScmObj* %args46586$k40487$1)
store volatile %struct.ScmObj* %args46586$k40487$2, %struct.ScmObj** %stackaddr$prim48133, align 8
%clofunc48134 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40487)
musttail call tailcc void %clofunc48134(%struct.ScmObj* %k40487, %struct.ScmObj* %args46586$k40487$2)
ret void
falsebranch$cmp48131:
%stackaddr$prim48135 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40115)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim48135, align 8
%stackaddr$makeclosure48136 = alloca %struct.ScmObj*, align 8
%fptrToInt48137 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40989 to i64
%ae40989 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48137)
store volatile %struct.ScmObj* %ae40989, %struct.ScmObj** %stackaddr$makeclosure48136, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40989, %struct.ScmObj* %k40487, i64 0)
%args46591$_37length40114$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48138 = alloca %struct.ScmObj*, align 8
%args46591$_37length40114$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40251, %struct.ScmObj* %args46591$_37length40114$0)
store volatile %struct.ScmObj* %args46591$_37length40114$1, %struct.ScmObj** %stackaddr$prim48138, align 8
%stackaddr$prim48139 = alloca %struct.ScmObj*, align 8
%args46591$_37length40114$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40989, %struct.ScmObj* %args46591$_37length40114$1)
store volatile %struct.ScmObj* %args46591$_37length40114$2, %struct.ScmObj** %stackaddr$prim48139, align 8
%clofunc48140 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40114)
musttail call tailcc void %clofunc48140(%struct.ScmObj* %_37length40114, %struct.ScmObj* %args46591$_37length40114$2)
ret void
}

define tailcc void @proc_clo$ae40989(%struct.ScmObj* %env$ae40989,%struct.ScmObj* %current_45args46587) {
%stackaddr$env-ref48141 = alloca %struct.ScmObj*, align 8
%k40487 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40989, i64 0)
store %struct.ScmObj* %k40487, %struct.ScmObj** %stackaddr$env-ref48141
%stackaddr$prim48142 = alloca %struct.ScmObj*, align 8
%_95k40488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46587)
store volatile %struct.ScmObj* %_95k40488, %struct.ScmObj** %stackaddr$prim48142, align 8
%stackaddr$prim48143 = alloca %struct.ScmObj*, align 8
%current_45args46588 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46587)
store volatile %struct.ScmObj* %current_45args46588, %struct.ScmObj** %stackaddr$prim48143, align 8
%stackaddr$prim48144 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46588)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim48144, align 8
%ae40991 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48145 = alloca %struct.ScmObj*, align 8
%cpsprim40489 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae40991, %struct.ScmObj* %anf_45bind40252)
store volatile %struct.ScmObj* %cpsprim40489, %struct.ScmObj** %stackaddr$prim48145, align 8
%ae40994 = call %struct.ScmObj* @const_init_int(i64 0)
%args46590$k40487$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48146 = alloca %struct.ScmObj*, align 8
%args46590$k40487$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40489, %struct.ScmObj* %args46590$k40487$0)
store volatile %struct.ScmObj* %args46590$k40487$1, %struct.ScmObj** %stackaddr$prim48146, align 8
%stackaddr$prim48147 = alloca %struct.ScmObj*, align 8
%args46590$k40487$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40994, %struct.ScmObj* %args46590$k40487$1)
store volatile %struct.ScmObj* %args46590$k40487$2, %struct.ScmObj** %stackaddr$prim48147, align 8
%clofunc48148 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40487)
musttail call tailcc void %clofunc48148(%struct.ScmObj* %k40487, %struct.ScmObj* %args46590$k40487$2)
ret void
}

define tailcc void @proc_clo$ae40822(%struct.ScmObj* %env$ae40822,%struct.ScmObj* %current_45args46595) {
%stackaddr$prim48149 = alloca %struct.ScmObj*, align 8
%k40490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46595)
store volatile %struct.ScmObj* %k40490, %struct.ScmObj** %stackaddr$prim48149, align 8
%stackaddr$prim48150 = alloca %struct.ScmObj*, align 8
%current_45args46596 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46595)
store volatile %struct.ScmObj* %current_45args46596, %struct.ScmObj** %stackaddr$prim48150, align 8
%stackaddr$prim48151 = alloca %struct.ScmObj*, align 8
%_37take40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46596)
store volatile %struct.ScmObj* %_37take40117, %struct.ScmObj** %stackaddr$prim48151, align 8
%ae40824 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48152 = alloca %struct.ScmObj*, align 8
%fptrToInt48153 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40825 to i64
%ae40825 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48153)
store volatile %struct.ScmObj* %ae40825, %struct.ScmObj** %stackaddr$makeclosure48152, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40825, %struct.ScmObj* %_37take40117, i64 0)
%args46609$k40490$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48154 = alloca %struct.ScmObj*, align 8
%args46609$k40490$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40825, %struct.ScmObj* %args46609$k40490$0)
store volatile %struct.ScmObj* %args46609$k40490$1, %struct.ScmObj** %stackaddr$prim48154, align 8
%stackaddr$prim48155 = alloca %struct.ScmObj*, align 8
%args46609$k40490$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40824, %struct.ScmObj* %args46609$k40490$1)
store volatile %struct.ScmObj* %args46609$k40490$2, %struct.ScmObj** %stackaddr$prim48155, align 8
%clofunc48156 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40490)
musttail call tailcc void %clofunc48156(%struct.ScmObj* %k40490, %struct.ScmObj* %args46609$k40490$2)
ret void
}

define tailcc void @proc_clo$ae40825(%struct.ScmObj* %env$ae40825,%struct.ScmObj* %current_45args46598) {
%stackaddr$env-ref48157 = alloca %struct.ScmObj*, align 8
%_37take40117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40825, i64 0)
store %struct.ScmObj* %_37take40117, %struct.ScmObj** %stackaddr$env-ref48157
%stackaddr$prim48158 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46598)
store volatile %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$prim48158, align 8
%stackaddr$prim48159 = alloca %struct.ScmObj*, align 8
%current_45args46599 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46598)
store volatile %struct.ScmObj* %current_45args46599, %struct.ScmObj** %stackaddr$prim48159, align 8
%stackaddr$prim48160 = alloca %struct.ScmObj*, align 8
%lst40119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46599)
store volatile %struct.ScmObj* %lst40119, %struct.ScmObj** %stackaddr$prim48160, align 8
%stackaddr$prim48161 = alloca %struct.ScmObj*, align 8
%current_45args46600 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46599)
store volatile %struct.ScmObj* %current_45args46600, %struct.ScmObj** %stackaddr$prim48161, align 8
%stackaddr$prim48162 = alloca %struct.ScmObj*, align 8
%n40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46600)
store volatile %struct.ScmObj* %n40118, %struct.ScmObj** %stackaddr$prim48162, align 8
%ae40827 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48163 = alloca %struct.ScmObj*, align 8
%anf_45bind40243 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40118, %struct.ScmObj* %ae40827)
store volatile %struct.ScmObj* %anf_45bind40243, %struct.ScmObj** %stackaddr$prim48163, align 8
%truthy$cmp48164 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40243)
%cmp$cmp48164 = icmp eq i64 %truthy$cmp48164, 1
br i1 %cmp$cmp48164, label %truebranch$cmp48164, label %falsebranch$cmp48164
truebranch$cmp48164:
%ae40830 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40831 = call %struct.ScmObj* @const_init_null()
%args46602$k40491$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48165 = alloca %struct.ScmObj*, align 8
%args46602$k40491$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40831, %struct.ScmObj* %args46602$k40491$0)
store volatile %struct.ScmObj* %args46602$k40491$1, %struct.ScmObj** %stackaddr$prim48165, align 8
%stackaddr$prim48166 = alloca %struct.ScmObj*, align 8
%args46602$k40491$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40830, %struct.ScmObj* %args46602$k40491$1)
store volatile %struct.ScmObj* %args46602$k40491$2, %struct.ScmObj** %stackaddr$prim48166, align 8
%clofunc48167 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40491)
musttail call tailcc void %clofunc48167(%struct.ScmObj* %k40491, %struct.ScmObj* %args46602$k40491$2)
ret void
falsebranch$cmp48164:
%stackaddr$prim48168 = alloca %struct.ScmObj*, align 8
%anf_45bind40244 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40119)
store volatile %struct.ScmObj* %anf_45bind40244, %struct.ScmObj** %stackaddr$prim48168, align 8
%truthy$cmp48169 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40244)
%cmp$cmp48169 = icmp eq i64 %truthy$cmp48169, 1
br i1 %cmp$cmp48169, label %truebranch$cmp48169, label %falsebranch$cmp48169
truebranch$cmp48169:
%ae40841 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40842 = call %struct.ScmObj* @const_init_null()
%args46603$k40491$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48170 = alloca %struct.ScmObj*, align 8
%args46603$k40491$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40842, %struct.ScmObj* %args46603$k40491$0)
store volatile %struct.ScmObj* %args46603$k40491$1, %struct.ScmObj** %stackaddr$prim48170, align 8
%stackaddr$prim48171 = alloca %struct.ScmObj*, align 8
%args46603$k40491$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40841, %struct.ScmObj* %args46603$k40491$1)
store volatile %struct.ScmObj* %args46603$k40491$2, %struct.ScmObj** %stackaddr$prim48171, align 8
%clofunc48172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40491)
musttail call tailcc void %clofunc48172(%struct.ScmObj* %k40491, %struct.ScmObj* %args46603$k40491$2)
ret void
falsebranch$cmp48169:
%stackaddr$prim48173 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40119)
store volatile %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$prim48173, align 8
%stackaddr$prim48174 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40119)
store volatile %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$prim48174, align 8
%ae40852 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48175 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40118, %struct.ScmObj* %ae40852)
store volatile %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$prim48175, align 8
%stackaddr$makeclosure48176 = alloca %struct.ScmObj*, align 8
%fptrToInt48177 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40854 to i64
%ae40854 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48177)
store volatile %struct.ScmObj* %ae40854, %struct.ScmObj** %stackaddr$makeclosure48176, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40854, %struct.ScmObj* %anf_45bind40245, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40854, %struct.ScmObj* %k40491, i64 1)
%args46608$_37take40117$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48178 = alloca %struct.ScmObj*, align 8
%args46608$_37take40117$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40247, %struct.ScmObj* %args46608$_37take40117$0)
store volatile %struct.ScmObj* %args46608$_37take40117$1, %struct.ScmObj** %stackaddr$prim48178, align 8
%stackaddr$prim48179 = alloca %struct.ScmObj*, align 8
%args46608$_37take40117$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40246, %struct.ScmObj* %args46608$_37take40117$1)
store volatile %struct.ScmObj* %args46608$_37take40117$2, %struct.ScmObj** %stackaddr$prim48179, align 8
%stackaddr$prim48180 = alloca %struct.ScmObj*, align 8
%args46608$_37take40117$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40854, %struct.ScmObj* %args46608$_37take40117$2)
store volatile %struct.ScmObj* %args46608$_37take40117$3, %struct.ScmObj** %stackaddr$prim48180, align 8
%clofunc48181 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40117)
musttail call tailcc void %clofunc48181(%struct.ScmObj* %_37take40117, %struct.ScmObj* %args46608$_37take40117$3)
ret void
}

define tailcc void @proc_clo$ae40854(%struct.ScmObj* %env$ae40854,%struct.ScmObj* %current_45args46604) {
%stackaddr$env-ref48182 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40854, i64 0)
store %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$env-ref48182
%stackaddr$env-ref48183 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40854, i64 1)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref48183
%stackaddr$prim48184 = alloca %struct.ScmObj*, align 8
%_95k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46604)
store volatile %struct.ScmObj* %_95k40492, %struct.ScmObj** %stackaddr$prim48184, align 8
%stackaddr$prim48185 = alloca %struct.ScmObj*, align 8
%current_45args46605 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46604)
store volatile %struct.ScmObj* %current_45args46605, %struct.ScmObj** %stackaddr$prim48185, align 8
%stackaddr$prim48186 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46605)
store volatile %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$prim48186, align 8
%stackaddr$prim48187 = alloca %struct.ScmObj*, align 8
%cpsprim40493 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40245, %struct.ScmObj* %anf_45bind40248)
store volatile %struct.ScmObj* %cpsprim40493, %struct.ScmObj** %stackaddr$prim48187, align 8
%ae40860 = call %struct.ScmObj* @const_init_int(i64 0)
%args46607$k40491$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48188 = alloca %struct.ScmObj*, align 8
%args46607$k40491$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40493, %struct.ScmObj* %args46607$k40491$0)
store volatile %struct.ScmObj* %args46607$k40491$1, %struct.ScmObj** %stackaddr$prim48188, align 8
%stackaddr$prim48189 = alloca %struct.ScmObj*, align 8
%args46607$k40491$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40860, %struct.ScmObj* %args46607$k40491$1)
store volatile %struct.ScmObj* %args46607$k40491$2, %struct.ScmObj** %stackaddr$prim48189, align 8
%clofunc48190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40491)
musttail call tailcc void %clofunc48190(%struct.ScmObj* %k40491, %struct.ScmObj* %args46607$k40491$2)
ret void
}

define tailcc void @proc_clo$ae40725(%struct.ScmObj* %env$ae40725,%struct.ScmObj* %current_45args46612) {
%stackaddr$prim48191 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46612)
store volatile %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$prim48191, align 8
%stackaddr$prim48192 = alloca %struct.ScmObj*, align 8
%current_45args46613 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46612)
store volatile %struct.ScmObj* %current_45args46613, %struct.ScmObj** %stackaddr$prim48192, align 8
%stackaddr$prim48193 = alloca %struct.ScmObj*, align 8
%_37map40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46613)
store volatile %struct.ScmObj* %_37map40121, %struct.ScmObj** %stackaddr$prim48193, align 8
%ae40727 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48194 = alloca %struct.ScmObj*, align 8
%fptrToInt48195 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40728 to i64
%ae40728 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48195)
store volatile %struct.ScmObj* %ae40728, %struct.ScmObj** %stackaddr$makeclosure48194, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40728, %struct.ScmObj* %_37map40121, i64 0)
%args46629$k40494$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48196 = alloca %struct.ScmObj*, align 8
%args46629$k40494$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40728, %struct.ScmObj* %args46629$k40494$0)
store volatile %struct.ScmObj* %args46629$k40494$1, %struct.ScmObj** %stackaddr$prim48196, align 8
%stackaddr$prim48197 = alloca %struct.ScmObj*, align 8
%args46629$k40494$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40727, %struct.ScmObj* %args46629$k40494$1)
store volatile %struct.ScmObj* %args46629$k40494$2, %struct.ScmObj** %stackaddr$prim48197, align 8
%clofunc48198 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40494)
musttail call tailcc void %clofunc48198(%struct.ScmObj* %k40494, %struct.ScmObj* %args46629$k40494$2)
ret void
}

define tailcc void @proc_clo$ae40728(%struct.ScmObj* %env$ae40728,%struct.ScmObj* %current_45args46615) {
%stackaddr$env-ref48199 = alloca %struct.ScmObj*, align 8
%_37map40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40728, i64 0)
store %struct.ScmObj* %_37map40121, %struct.ScmObj** %stackaddr$env-ref48199
%stackaddr$prim48200 = alloca %struct.ScmObj*, align 8
%k40495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46615)
store volatile %struct.ScmObj* %k40495, %struct.ScmObj** %stackaddr$prim48200, align 8
%stackaddr$prim48201 = alloca %struct.ScmObj*, align 8
%current_45args46616 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46615)
store volatile %struct.ScmObj* %current_45args46616, %struct.ScmObj** %stackaddr$prim48201, align 8
%stackaddr$prim48202 = alloca %struct.ScmObj*, align 8
%f40123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46616)
store volatile %struct.ScmObj* %f40123, %struct.ScmObj** %stackaddr$prim48202, align 8
%stackaddr$prim48203 = alloca %struct.ScmObj*, align 8
%current_45args46617 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46616)
store volatile %struct.ScmObj* %current_45args46617, %struct.ScmObj** %stackaddr$prim48203, align 8
%stackaddr$prim48204 = alloca %struct.ScmObj*, align 8
%lst40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46617)
store volatile %struct.ScmObj* %lst40122, %struct.ScmObj** %stackaddr$prim48204, align 8
%stackaddr$prim48205 = alloca %struct.ScmObj*, align 8
%anf_45bind40237 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40237, %struct.ScmObj** %stackaddr$prim48205, align 8
%truthy$cmp48206 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40237)
%cmp$cmp48206 = icmp eq i64 %truthy$cmp48206, 1
br i1 %cmp$cmp48206, label %truebranch$cmp48206, label %falsebranch$cmp48206
truebranch$cmp48206:
%ae40732 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40733 = call %struct.ScmObj* @const_init_null()
%args46619$k40495$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48207 = alloca %struct.ScmObj*, align 8
%args46619$k40495$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40733, %struct.ScmObj* %args46619$k40495$0)
store volatile %struct.ScmObj* %args46619$k40495$1, %struct.ScmObj** %stackaddr$prim48207, align 8
%stackaddr$prim48208 = alloca %struct.ScmObj*, align 8
%args46619$k40495$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40732, %struct.ScmObj* %args46619$k40495$1)
store volatile %struct.ScmObj* %args46619$k40495$2, %struct.ScmObj** %stackaddr$prim48208, align 8
%clofunc48209 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40495)
musttail call tailcc void %clofunc48209(%struct.ScmObj* %k40495, %struct.ScmObj* %args46619$k40495$2)
ret void
falsebranch$cmp48206:
%stackaddr$prim48210 = alloca %struct.ScmObj*, align 8
%anf_45bind40238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40238, %struct.ScmObj** %stackaddr$prim48210, align 8
%stackaddr$makeclosure48211 = alloca %struct.ScmObj*, align 8
%fptrToInt48212 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40742 to i64
%ae40742 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48212)
store volatile %struct.ScmObj* %ae40742, %struct.ScmObj** %stackaddr$makeclosure48211, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40742, %struct.ScmObj* %_37map40121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40742, %struct.ScmObj* %k40495, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40742, %struct.ScmObj* %f40123, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40742, %struct.ScmObj* %lst40122, i64 3)
%args46628$f40123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48213 = alloca %struct.ScmObj*, align 8
%args46628$f40123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40238, %struct.ScmObj* %args46628$f40123$0)
store volatile %struct.ScmObj* %args46628$f40123$1, %struct.ScmObj** %stackaddr$prim48213, align 8
%stackaddr$prim48214 = alloca %struct.ScmObj*, align 8
%args46628$f40123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40742, %struct.ScmObj* %args46628$f40123$1)
store volatile %struct.ScmObj* %args46628$f40123$2, %struct.ScmObj** %stackaddr$prim48214, align 8
%clofunc48215 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40123)
musttail call tailcc void %clofunc48215(%struct.ScmObj* %f40123, %struct.ScmObj* %args46628$f40123$2)
ret void
}

define tailcc void @proc_clo$ae40742(%struct.ScmObj* %env$ae40742,%struct.ScmObj* %current_45args46620) {
%stackaddr$env-ref48216 = alloca %struct.ScmObj*, align 8
%_37map40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40742, i64 0)
store %struct.ScmObj* %_37map40121, %struct.ScmObj** %stackaddr$env-ref48216
%stackaddr$env-ref48217 = alloca %struct.ScmObj*, align 8
%k40495 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40742, i64 1)
store %struct.ScmObj* %k40495, %struct.ScmObj** %stackaddr$env-ref48217
%stackaddr$env-ref48218 = alloca %struct.ScmObj*, align 8
%f40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40742, i64 2)
store %struct.ScmObj* %f40123, %struct.ScmObj** %stackaddr$env-ref48218
%stackaddr$env-ref48219 = alloca %struct.ScmObj*, align 8
%lst40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40742, i64 3)
store %struct.ScmObj* %lst40122, %struct.ScmObj** %stackaddr$env-ref48219
%stackaddr$prim48220 = alloca %struct.ScmObj*, align 8
%_95k40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46620)
store volatile %struct.ScmObj* %_95k40496, %struct.ScmObj** %stackaddr$prim48220, align 8
%stackaddr$prim48221 = alloca %struct.ScmObj*, align 8
%current_45args46621 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46620)
store volatile %struct.ScmObj* %current_45args46621, %struct.ScmObj** %stackaddr$prim48221, align 8
%stackaddr$prim48222 = alloca %struct.ScmObj*, align 8
%anf_45bind40239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46621)
store volatile %struct.ScmObj* %anf_45bind40239, %struct.ScmObj** %stackaddr$prim48222, align 8
%stackaddr$prim48223 = alloca %struct.ScmObj*, align 8
%anf_45bind40240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40240, %struct.ScmObj** %stackaddr$prim48223, align 8
%stackaddr$makeclosure48224 = alloca %struct.ScmObj*, align 8
%fptrToInt48225 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40746 to i64
%ae40746 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48225)
store volatile %struct.ScmObj* %ae40746, %struct.ScmObj** %stackaddr$makeclosure48224, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40746, %struct.ScmObj* %k40495, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40746, %struct.ScmObj* %anf_45bind40239, i64 1)
%args46627$_37map40121$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48226 = alloca %struct.ScmObj*, align 8
%args46627$_37map40121$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40240, %struct.ScmObj* %args46627$_37map40121$0)
store volatile %struct.ScmObj* %args46627$_37map40121$1, %struct.ScmObj** %stackaddr$prim48226, align 8
%stackaddr$prim48227 = alloca %struct.ScmObj*, align 8
%args46627$_37map40121$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40123, %struct.ScmObj* %args46627$_37map40121$1)
store volatile %struct.ScmObj* %args46627$_37map40121$2, %struct.ScmObj** %stackaddr$prim48227, align 8
%stackaddr$prim48228 = alloca %struct.ScmObj*, align 8
%args46627$_37map40121$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40746, %struct.ScmObj* %args46627$_37map40121$2)
store volatile %struct.ScmObj* %args46627$_37map40121$3, %struct.ScmObj** %stackaddr$prim48228, align 8
%clofunc48229 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40121)
musttail call tailcc void %clofunc48229(%struct.ScmObj* %_37map40121, %struct.ScmObj* %args46627$_37map40121$3)
ret void
}

define tailcc void @proc_clo$ae40746(%struct.ScmObj* %env$ae40746,%struct.ScmObj* %current_45args46623) {
%stackaddr$env-ref48230 = alloca %struct.ScmObj*, align 8
%k40495 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40746, i64 0)
store %struct.ScmObj* %k40495, %struct.ScmObj** %stackaddr$env-ref48230
%stackaddr$env-ref48231 = alloca %struct.ScmObj*, align 8
%anf_45bind40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40746, i64 1)
store %struct.ScmObj* %anf_45bind40239, %struct.ScmObj** %stackaddr$env-ref48231
%stackaddr$prim48232 = alloca %struct.ScmObj*, align 8
%_95k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46623)
store volatile %struct.ScmObj* %_95k40497, %struct.ScmObj** %stackaddr$prim48232, align 8
%stackaddr$prim48233 = alloca %struct.ScmObj*, align 8
%current_45args46624 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46623)
store volatile %struct.ScmObj* %current_45args46624, %struct.ScmObj** %stackaddr$prim48233, align 8
%stackaddr$prim48234 = alloca %struct.ScmObj*, align 8
%anf_45bind40241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46624)
store volatile %struct.ScmObj* %anf_45bind40241, %struct.ScmObj** %stackaddr$prim48234, align 8
%stackaddr$prim48235 = alloca %struct.ScmObj*, align 8
%cpsprim40498 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40239, %struct.ScmObj* %anf_45bind40241)
store volatile %struct.ScmObj* %cpsprim40498, %struct.ScmObj** %stackaddr$prim48235, align 8
%ae40752 = call %struct.ScmObj* @const_init_int(i64 0)
%args46626$k40495$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48236 = alloca %struct.ScmObj*, align 8
%args46626$k40495$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40498, %struct.ScmObj* %args46626$k40495$0)
store volatile %struct.ScmObj* %args46626$k40495$1, %struct.ScmObj** %stackaddr$prim48236, align 8
%stackaddr$prim48237 = alloca %struct.ScmObj*, align 8
%args46626$k40495$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40752, %struct.ScmObj* %args46626$k40495$1)
store volatile %struct.ScmObj* %args46626$k40495$2, %struct.ScmObj** %stackaddr$prim48237, align 8
%clofunc48238 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40495)
musttail call tailcc void %clofunc48238(%struct.ScmObj* %k40495, %struct.ScmObj* %args46626$k40495$2)
ret void
}

define tailcc void @proc_clo$ae40645(%struct.ScmObj* %env$ae40645,%struct.ScmObj* %current_45args46632) {
%stackaddr$prim48239 = alloca %struct.ScmObj*, align 8
%k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46632)
store volatile %struct.ScmObj* %k40499, %struct.ScmObj** %stackaddr$prim48239, align 8
%stackaddr$prim48240 = alloca %struct.ScmObj*, align 8
%current_45args46633 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46632)
store volatile %struct.ScmObj* %current_45args46633, %struct.ScmObj** %stackaddr$prim48240, align 8
%stackaddr$prim48241 = alloca %struct.ScmObj*, align 8
%_37foldr140125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46633)
store volatile %struct.ScmObj* %_37foldr140125, %struct.ScmObj** %stackaddr$prim48241, align 8
%ae40647 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48242 = alloca %struct.ScmObj*, align 8
%fptrToInt48243 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40648 to i64
%ae40648 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48243)
store volatile %struct.ScmObj* %ae40648, %struct.ScmObj** %stackaddr$makeclosure48242, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40648, %struct.ScmObj* %_37foldr140125, i64 0)
%args46646$k40499$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48244 = alloca %struct.ScmObj*, align 8
%args46646$k40499$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40648, %struct.ScmObj* %args46646$k40499$0)
store volatile %struct.ScmObj* %args46646$k40499$1, %struct.ScmObj** %stackaddr$prim48244, align 8
%stackaddr$prim48245 = alloca %struct.ScmObj*, align 8
%args46646$k40499$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40647, %struct.ScmObj* %args46646$k40499$1)
store volatile %struct.ScmObj* %args46646$k40499$2, %struct.ScmObj** %stackaddr$prim48245, align 8
%clofunc48246 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40499)
musttail call tailcc void %clofunc48246(%struct.ScmObj* %k40499, %struct.ScmObj* %args46646$k40499$2)
ret void
}

define tailcc void @proc_clo$ae40648(%struct.ScmObj* %env$ae40648,%struct.ScmObj* %current_45args46635) {
%stackaddr$env-ref48247 = alloca %struct.ScmObj*, align 8
%_37foldr140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40648, i64 0)
store %struct.ScmObj* %_37foldr140125, %struct.ScmObj** %stackaddr$env-ref48247
%stackaddr$prim48248 = alloca %struct.ScmObj*, align 8
%k40500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46635)
store volatile %struct.ScmObj* %k40500, %struct.ScmObj** %stackaddr$prim48248, align 8
%stackaddr$prim48249 = alloca %struct.ScmObj*, align 8
%current_45args46636 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46635)
store volatile %struct.ScmObj* %current_45args46636, %struct.ScmObj** %stackaddr$prim48249, align 8
%stackaddr$prim48250 = alloca %struct.ScmObj*, align 8
%f40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46636)
store volatile %struct.ScmObj* %f40128, %struct.ScmObj** %stackaddr$prim48250, align 8
%stackaddr$prim48251 = alloca %struct.ScmObj*, align 8
%current_45args46637 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46636)
store volatile %struct.ScmObj* %current_45args46637, %struct.ScmObj** %stackaddr$prim48251, align 8
%stackaddr$prim48252 = alloca %struct.ScmObj*, align 8
%acc40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46637)
store volatile %struct.ScmObj* %acc40127, %struct.ScmObj** %stackaddr$prim48252, align 8
%stackaddr$prim48253 = alloca %struct.ScmObj*, align 8
%current_45args46638 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46637)
store volatile %struct.ScmObj* %current_45args46638, %struct.ScmObj** %stackaddr$prim48253, align 8
%stackaddr$prim48254 = alloca %struct.ScmObj*, align 8
%lst40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46638)
store volatile %struct.ScmObj* %lst40126, %struct.ScmObj** %stackaddr$prim48254, align 8
%stackaddr$prim48255 = alloca %struct.ScmObj*, align 8
%anf_45bind40232 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40232, %struct.ScmObj** %stackaddr$prim48255, align 8
%truthy$cmp48256 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40232)
%cmp$cmp48256 = icmp eq i64 %truthy$cmp48256, 1
br i1 %cmp$cmp48256, label %truebranch$cmp48256, label %falsebranch$cmp48256
truebranch$cmp48256:
%ae40652 = call %struct.ScmObj* @const_init_int(i64 0)
%args46640$k40500$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48257 = alloca %struct.ScmObj*, align 8
%args46640$k40500$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40127, %struct.ScmObj* %args46640$k40500$0)
store volatile %struct.ScmObj* %args46640$k40500$1, %struct.ScmObj** %stackaddr$prim48257, align 8
%stackaddr$prim48258 = alloca %struct.ScmObj*, align 8
%args46640$k40500$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40652, %struct.ScmObj* %args46640$k40500$1)
store volatile %struct.ScmObj* %args46640$k40500$2, %struct.ScmObj** %stackaddr$prim48258, align 8
%clofunc48259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40500)
musttail call tailcc void %clofunc48259(%struct.ScmObj* %k40500, %struct.ScmObj* %args46640$k40500$2)
ret void
falsebranch$cmp48256:
%stackaddr$prim48260 = alloca %struct.ScmObj*, align 8
%anf_45bind40233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40233, %struct.ScmObj** %stackaddr$prim48260, align 8
%stackaddr$prim48261 = alloca %struct.ScmObj*, align 8
%anf_45bind40234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40234, %struct.ScmObj** %stackaddr$prim48261, align 8
%stackaddr$makeclosure48262 = alloca %struct.ScmObj*, align 8
%fptrToInt48263 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40660 to i64
%ae40660 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48263)
store volatile %struct.ScmObj* %ae40660, %struct.ScmObj** %stackaddr$makeclosure48262, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40660, %struct.ScmObj* %anf_45bind40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40660, %struct.ScmObj* %k40500, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40660, %struct.ScmObj* %f40128, i64 2)
%args46645$_37foldr140125$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48264 = alloca %struct.ScmObj*, align 8
%args46645$_37foldr140125$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40234, %struct.ScmObj* %args46645$_37foldr140125$0)
store volatile %struct.ScmObj* %args46645$_37foldr140125$1, %struct.ScmObj** %stackaddr$prim48264, align 8
%stackaddr$prim48265 = alloca %struct.ScmObj*, align 8
%args46645$_37foldr140125$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40127, %struct.ScmObj* %args46645$_37foldr140125$1)
store volatile %struct.ScmObj* %args46645$_37foldr140125$2, %struct.ScmObj** %stackaddr$prim48265, align 8
%stackaddr$prim48266 = alloca %struct.ScmObj*, align 8
%args46645$_37foldr140125$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40128, %struct.ScmObj* %args46645$_37foldr140125$2)
store volatile %struct.ScmObj* %args46645$_37foldr140125$3, %struct.ScmObj** %stackaddr$prim48266, align 8
%stackaddr$prim48267 = alloca %struct.ScmObj*, align 8
%args46645$_37foldr140125$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40660, %struct.ScmObj* %args46645$_37foldr140125$3)
store volatile %struct.ScmObj* %args46645$_37foldr140125$4, %struct.ScmObj** %stackaddr$prim48267, align 8
%clofunc48268 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140125)
musttail call tailcc void %clofunc48268(%struct.ScmObj* %_37foldr140125, %struct.ScmObj* %args46645$_37foldr140125$4)
ret void
}

define tailcc void @proc_clo$ae40660(%struct.ScmObj* %env$ae40660,%struct.ScmObj* %current_45args46641) {
%stackaddr$env-ref48269 = alloca %struct.ScmObj*, align 8
%anf_45bind40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40660, i64 0)
store %struct.ScmObj* %anf_45bind40233, %struct.ScmObj** %stackaddr$env-ref48269
%stackaddr$env-ref48270 = alloca %struct.ScmObj*, align 8
%k40500 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40660, i64 1)
store %struct.ScmObj* %k40500, %struct.ScmObj** %stackaddr$env-ref48270
%stackaddr$env-ref48271 = alloca %struct.ScmObj*, align 8
%f40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40660, i64 2)
store %struct.ScmObj* %f40128, %struct.ScmObj** %stackaddr$env-ref48271
%stackaddr$prim48272 = alloca %struct.ScmObj*, align 8
%_95k40501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46641)
store volatile %struct.ScmObj* %_95k40501, %struct.ScmObj** %stackaddr$prim48272, align 8
%stackaddr$prim48273 = alloca %struct.ScmObj*, align 8
%current_45args46642 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46641)
store volatile %struct.ScmObj* %current_45args46642, %struct.ScmObj** %stackaddr$prim48273, align 8
%stackaddr$prim48274 = alloca %struct.ScmObj*, align 8
%anf_45bind40235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46642)
store volatile %struct.ScmObj* %anf_45bind40235, %struct.ScmObj** %stackaddr$prim48274, align 8
%args46644$f40128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48275 = alloca %struct.ScmObj*, align 8
%args46644$f40128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40235, %struct.ScmObj* %args46644$f40128$0)
store volatile %struct.ScmObj* %args46644$f40128$1, %struct.ScmObj** %stackaddr$prim48275, align 8
%stackaddr$prim48276 = alloca %struct.ScmObj*, align 8
%args46644$f40128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40233, %struct.ScmObj* %args46644$f40128$1)
store volatile %struct.ScmObj* %args46644$f40128$2, %struct.ScmObj** %stackaddr$prim48276, align 8
%stackaddr$prim48277 = alloca %struct.ScmObj*, align 8
%args46644$f40128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40500, %struct.ScmObj* %args46644$f40128$2)
store volatile %struct.ScmObj* %args46644$f40128$3, %struct.ScmObj** %stackaddr$prim48277, align 8
%clofunc48278 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40128)
musttail call tailcc void %clofunc48278(%struct.ScmObj* %f40128, %struct.ScmObj* %args46644$f40128$3)
ret void
}

define tailcc void @proc_clo$ae40528(%struct.ScmObj* %env$ae40528,%struct.ScmObj* %current_45args46649) {
%stackaddr$prim48279 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46649)
store volatile %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$prim48279, align 8
%stackaddr$prim48280 = alloca %struct.ScmObj*, align 8
%current_45args46650 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46649)
store volatile %struct.ScmObj* %current_45args46650, %struct.ScmObj** %stackaddr$prim48280, align 8
%stackaddr$prim48281 = alloca %struct.ScmObj*, align 8
%y40105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46650)
store volatile %struct.ScmObj* %y40105, %struct.ScmObj** %stackaddr$prim48281, align 8
%ae40530 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48282 = alloca %struct.ScmObj*, align 8
%fptrToInt48283 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40531 to i64
%ae40531 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48283)
store volatile %struct.ScmObj* %ae40531, %struct.ScmObj** %stackaddr$makeclosure48282, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40531, %struct.ScmObj* %y40105, i64 0)
%args46668$k40502$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48284 = alloca %struct.ScmObj*, align 8
%args46668$k40502$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40531, %struct.ScmObj* %args46668$k40502$0)
store volatile %struct.ScmObj* %args46668$k40502$1, %struct.ScmObj** %stackaddr$prim48284, align 8
%stackaddr$prim48285 = alloca %struct.ScmObj*, align 8
%args46668$k40502$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40530, %struct.ScmObj* %args46668$k40502$1)
store volatile %struct.ScmObj* %args46668$k40502$2, %struct.ScmObj** %stackaddr$prim48285, align 8
%clofunc48286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40502)
musttail call tailcc void %clofunc48286(%struct.ScmObj* %k40502, %struct.ScmObj* %args46668$k40502$2)
ret void
}

define tailcc void @proc_clo$ae40531(%struct.ScmObj* %env$ae40531,%struct.ScmObj* %current_45args46652) {
%stackaddr$env-ref48287 = alloca %struct.ScmObj*, align 8
%y40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40531, i64 0)
store %struct.ScmObj* %y40105, %struct.ScmObj** %stackaddr$env-ref48287
%stackaddr$prim48288 = alloca %struct.ScmObj*, align 8
%k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46652)
store volatile %struct.ScmObj* %k40503, %struct.ScmObj** %stackaddr$prim48288, align 8
%stackaddr$prim48289 = alloca %struct.ScmObj*, align 8
%current_45args46653 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46652)
store volatile %struct.ScmObj* %current_45args46653, %struct.ScmObj** %stackaddr$prim48289, align 8
%stackaddr$prim48290 = alloca %struct.ScmObj*, align 8
%f40106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46653)
store volatile %struct.ScmObj* %f40106, %struct.ScmObj** %stackaddr$prim48290, align 8
%stackaddr$makeclosure48291 = alloca %struct.ScmObj*, align 8
%fptrToInt48292 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40532 to i64
%ae40532 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48292)
store volatile %struct.ScmObj* %ae40532, %struct.ScmObj** %stackaddr$makeclosure48291, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40532, %struct.ScmObj* %k40503, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40532, %struct.ScmObj* %f40106, i64 1)
%ae40533 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48293 = alloca %struct.ScmObj*, align 8
%fptrToInt48294 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40534 to i64
%ae40534 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48294)
store volatile %struct.ScmObj* %ae40534, %struct.ScmObj** %stackaddr$makeclosure48293, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40534, %struct.ScmObj* %y40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40534, %struct.ScmObj* %f40106, i64 1)
%args46667$ae40532$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48295 = alloca %struct.ScmObj*, align 8
%args46667$ae40532$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40534, %struct.ScmObj* %args46667$ae40532$0)
store volatile %struct.ScmObj* %args46667$ae40532$1, %struct.ScmObj** %stackaddr$prim48295, align 8
%stackaddr$prim48296 = alloca %struct.ScmObj*, align 8
%args46667$ae40532$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40533, %struct.ScmObj* %args46667$ae40532$1)
store volatile %struct.ScmObj* %args46667$ae40532$2, %struct.ScmObj** %stackaddr$prim48296, align 8
%clofunc48297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40532)
musttail call tailcc void %clofunc48297(%struct.ScmObj* %ae40532, %struct.ScmObj* %args46667$ae40532$2)
ret void
}

define tailcc void @proc_clo$ae40532(%struct.ScmObj* %env$ae40532,%struct.ScmObj* %current_45args46655) {
%stackaddr$env-ref48298 = alloca %struct.ScmObj*, align 8
%k40503 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40532, i64 0)
store %struct.ScmObj* %k40503, %struct.ScmObj** %stackaddr$env-ref48298
%stackaddr$env-ref48299 = alloca %struct.ScmObj*, align 8
%f40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40532, i64 1)
store %struct.ScmObj* %f40106, %struct.ScmObj** %stackaddr$env-ref48299
%stackaddr$prim48300 = alloca %struct.ScmObj*, align 8
%_95k40504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46655)
store volatile %struct.ScmObj* %_95k40504, %struct.ScmObj** %stackaddr$prim48300, align 8
%stackaddr$prim48301 = alloca %struct.ScmObj*, align 8
%current_45args46656 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46655)
store volatile %struct.ScmObj* %current_45args46656, %struct.ScmObj** %stackaddr$prim48301, align 8
%stackaddr$prim48302 = alloca %struct.ScmObj*, align 8
%anf_45bind40230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46656)
store volatile %struct.ScmObj* %anf_45bind40230, %struct.ScmObj** %stackaddr$prim48302, align 8
%args46658$f40106$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48303 = alloca %struct.ScmObj*, align 8
%args46658$f40106$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40230, %struct.ScmObj* %args46658$f40106$0)
store volatile %struct.ScmObj* %args46658$f40106$1, %struct.ScmObj** %stackaddr$prim48303, align 8
%stackaddr$prim48304 = alloca %struct.ScmObj*, align 8
%args46658$f40106$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40503, %struct.ScmObj* %args46658$f40106$1)
store volatile %struct.ScmObj* %args46658$f40106$2, %struct.ScmObj** %stackaddr$prim48304, align 8
%clofunc48305 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40106)
musttail call tailcc void %clofunc48305(%struct.ScmObj* %f40106, %struct.ScmObj* %args46658$f40106$2)
ret void
}

define tailcc void @proc_clo$ae40534(%struct.ScmObj* %env$ae40534,%struct.ScmObj* %args4010740505) {
%stackaddr$env-ref48306 = alloca %struct.ScmObj*, align 8
%y40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40534, i64 0)
store %struct.ScmObj* %y40105, %struct.ScmObj** %stackaddr$env-ref48306
%stackaddr$env-ref48307 = alloca %struct.ScmObj*, align 8
%f40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40534, i64 1)
store %struct.ScmObj* %f40106, %struct.ScmObj** %stackaddr$env-ref48307
%stackaddr$prim48308 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4010740505)
store volatile %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$prim48308, align 8
%stackaddr$prim48309 = alloca %struct.ScmObj*, align 8
%args40107 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4010740505)
store volatile %struct.ScmObj* %args40107, %struct.ScmObj** %stackaddr$prim48309, align 8
%stackaddr$makeclosure48310 = alloca %struct.ScmObj*, align 8
%fptrToInt48311 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40538 to i64
%ae40538 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48311)
store volatile %struct.ScmObj* %ae40538, %struct.ScmObj** %stackaddr$makeclosure48310, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40538, %struct.ScmObj* %k40506, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40538, %struct.ScmObj* %f40106, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40538, %struct.ScmObj* %args40107, i64 2)
%args46666$y40105$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48312 = alloca %struct.ScmObj*, align 8
%args46666$y40105$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40105, %struct.ScmObj* %args46666$y40105$0)
store volatile %struct.ScmObj* %args46666$y40105$1, %struct.ScmObj** %stackaddr$prim48312, align 8
%stackaddr$prim48313 = alloca %struct.ScmObj*, align 8
%args46666$y40105$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40538, %struct.ScmObj* %args46666$y40105$1)
store volatile %struct.ScmObj* %args46666$y40105$2, %struct.ScmObj** %stackaddr$prim48313, align 8
%clofunc48314 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40105)
musttail call tailcc void %clofunc48314(%struct.ScmObj* %y40105, %struct.ScmObj* %args46666$y40105$2)
ret void
}

define tailcc void @proc_clo$ae40538(%struct.ScmObj* %env$ae40538,%struct.ScmObj* %current_45args46659) {
%stackaddr$env-ref48315 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40538, i64 0)
store %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$env-ref48315
%stackaddr$env-ref48316 = alloca %struct.ScmObj*, align 8
%f40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40538, i64 1)
store %struct.ScmObj* %f40106, %struct.ScmObj** %stackaddr$env-ref48316
%stackaddr$env-ref48317 = alloca %struct.ScmObj*, align 8
%args40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40538, i64 2)
store %struct.ScmObj* %args40107, %struct.ScmObj** %stackaddr$env-ref48317
%stackaddr$prim48318 = alloca %struct.ScmObj*, align 8
%_95k40507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46659)
store volatile %struct.ScmObj* %_95k40507, %struct.ScmObj** %stackaddr$prim48318, align 8
%stackaddr$prim48319 = alloca %struct.ScmObj*, align 8
%current_45args46660 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46659)
store volatile %struct.ScmObj* %current_45args46660, %struct.ScmObj** %stackaddr$prim48319, align 8
%stackaddr$prim48320 = alloca %struct.ScmObj*, align 8
%anf_45bind40228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46660)
store volatile %struct.ScmObj* %anf_45bind40228, %struct.ScmObj** %stackaddr$prim48320, align 8
%stackaddr$makeclosure48321 = alloca %struct.ScmObj*, align 8
%fptrToInt48322 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40541 to i64
%ae40541 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48322)
store volatile %struct.ScmObj* %ae40541, %struct.ScmObj** %stackaddr$makeclosure48321, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40541, %struct.ScmObj* %args40107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40541, %struct.ScmObj* %k40506, i64 1)
%args46665$anf_45bind40228$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48323 = alloca %struct.ScmObj*, align 8
%args46665$anf_45bind40228$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40106, %struct.ScmObj* %args46665$anf_45bind40228$0)
store volatile %struct.ScmObj* %args46665$anf_45bind40228$1, %struct.ScmObj** %stackaddr$prim48323, align 8
%stackaddr$prim48324 = alloca %struct.ScmObj*, align 8
%args46665$anf_45bind40228$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40541, %struct.ScmObj* %args46665$anf_45bind40228$1)
store volatile %struct.ScmObj* %args46665$anf_45bind40228$2, %struct.ScmObj** %stackaddr$prim48324, align 8
%clofunc48325 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40228)
musttail call tailcc void %clofunc48325(%struct.ScmObj* %anf_45bind40228, %struct.ScmObj* %args46665$anf_45bind40228$2)
ret void
}

define tailcc void @proc_clo$ae40541(%struct.ScmObj* %env$ae40541,%struct.ScmObj* %current_45args46662) {
%stackaddr$env-ref48326 = alloca %struct.ScmObj*, align 8
%args40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40541, i64 0)
store %struct.ScmObj* %args40107, %struct.ScmObj** %stackaddr$env-ref48326
%stackaddr$env-ref48327 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40541, i64 1)
store %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$env-ref48327
%stackaddr$prim48328 = alloca %struct.ScmObj*, align 8
%_95k40508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46662)
store volatile %struct.ScmObj* %_95k40508, %struct.ScmObj** %stackaddr$prim48328, align 8
%stackaddr$prim48329 = alloca %struct.ScmObj*, align 8
%current_45args46663 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46662)
store volatile %struct.ScmObj* %current_45args46663, %struct.ScmObj** %stackaddr$prim48329, align 8
%stackaddr$prim48330 = alloca %struct.ScmObj*, align 8
%anf_45bind40229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46663)
store volatile %struct.ScmObj* %anf_45bind40229, %struct.ScmObj** %stackaddr$prim48330, align 8
%stackaddr$prim48331 = alloca %struct.ScmObj*, align 8
%cpsargs40509 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40506, %struct.ScmObj* %args40107)
store volatile %struct.ScmObj* %cpsargs40509, %struct.ScmObj** %stackaddr$prim48331, align 8
%clofunc48332 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40229)
musttail call tailcc void %clofunc48332(%struct.ScmObj* %anf_45bind40229, %struct.ScmObj* %cpsargs40509)
ret void
}

define tailcc void @proc_clo$ae40513(%struct.ScmObj* %env$ae40513,%struct.ScmObj* %current_45args46670) {
%stackaddr$prim48333 = alloca %struct.ScmObj*, align 8
%k40510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46670)
store volatile %struct.ScmObj* %k40510, %struct.ScmObj** %stackaddr$prim48333, align 8
%stackaddr$prim48334 = alloca %struct.ScmObj*, align 8
%current_45args46671 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46670)
store volatile %struct.ScmObj* %current_45args46671, %struct.ScmObj** %stackaddr$prim48334, align 8
%stackaddr$prim48335 = alloca %struct.ScmObj*, align 8
%yu40104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46671)
store volatile %struct.ScmObj* %yu40104, %struct.ScmObj** %stackaddr$prim48335, align 8
%args46673$yu40104$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48336 = alloca %struct.ScmObj*, align 8
%args46673$yu40104$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40104, %struct.ScmObj* %args46673$yu40104$0)
store volatile %struct.ScmObj* %args46673$yu40104$1, %struct.ScmObj** %stackaddr$prim48336, align 8
%stackaddr$prim48337 = alloca %struct.ScmObj*, align 8
%args46673$yu40104$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40510, %struct.ScmObj* %args46673$yu40104$1)
store volatile %struct.ScmObj* %args46673$yu40104$2, %struct.ScmObj** %stackaddr$prim48337, align 8
%clofunc48338 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40104)
musttail call tailcc void %clofunc48338(%struct.ScmObj* %yu40104, %struct.ScmObj* %args46673$yu40104$2)
ret void
}