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
%mainenv55539 = call %struct.ScmObj* @const_init_null()
%mainargs55540 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv55539, %struct.ScmObj* %mainargs55540)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv55537,%struct.ScmObj* %mainargs55538) {
%stackaddr$makeclosure55541 = alloca %struct.ScmObj*, align 8
%fptrToInt55542 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48501 to i64
%ae48501 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55542)
store volatile %struct.ScmObj* %ae48501, %struct.ScmObj** %stackaddr$makeclosure55541, align 8
%ae48502 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55543 = alloca %struct.ScmObj*, align 8
%fptrToInt55544 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48503 to i64
%ae48503 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55544)
store volatile %struct.ScmObj* %ae48503, %struct.ScmObj** %stackaddr$makeclosure55543, align 8
%argslist55536$ae485010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55545 = alloca %struct.ScmObj*, align 8
%argslist55536$ae485011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48503, %struct.ScmObj* %argslist55536$ae485010)
store volatile %struct.ScmObj* %argslist55536$ae485011, %struct.ScmObj** %stackaddr$prim55545, align 8
%stackaddr$prim55546 = alloca %struct.ScmObj*, align 8
%argslist55536$ae485012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48502, %struct.ScmObj* %argslist55536$ae485011)
store volatile %struct.ScmObj* %argslist55536$ae485012, %struct.ScmObj** %stackaddr$prim55546, align 8
%clofunc55547 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48501)
musttail call tailcc void %clofunc55547(%struct.ScmObj* %ae48501, %struct.ScmObj* %argslist55536$ae485012)
ret void
}

define tailcc void @proc_clo$ae48501(%struct.ScmObj* %env$ae48501,%struct.ScmObj* %current_45args54914) {
%stackaddr$prim55548 = alloca %struct.ScmObj*, align 8
%_95k48299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54914)
store volatile %struct.ScmObj* %_95k48299, %struct.ScmObj** %stackaddr$prim55548, align 8
%stackaddr$prim55549 = alloca %struct.ScmObj*, align 8
%current_45args54915 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54914)
store volatile %struct.ScmObj* %current_45args54915, %struct.ScmObj** %stackaddr$prim55549, align 8
%stackaddr$prim55550 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54915)
store volatile %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$prim55550, align 8
%stackaddr$makeclosure55551 = alloca %struct.ScmObj*, align 8
%fptrToInt55552 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48516 to i64
%ae48516 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55552)
store volatile %struct.ScmObj* %ae48516, %struct.ScmObj** %stackaddr$makeclosure55551, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48516, %struct.ScmObj* %anf_45bind48164, i64 0)
%ae48517 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55553 = alloca %struct.ScmObj*, align 8
%fptrToInt55554 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48518 to i64
%ae48518 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55554)
store volatile %struct.ScmObj* %ae48518, %struct.ScmObj** %stackaddr$makeclosure55553, align 8
%argslist55531$ae485160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55555 = alloca %struct.ScmObj*, align 8
%argslist55531$ae485161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48518, %struct.ScmObj* %argslist55531$ae485160)
store volatile %struct.ScmObj* %argslist55531$ae485161, %struct.ScmObj** %stackaddr$prim55555, align 8
%stackaddr$prim55556 = alloca %struct.ScmObj*, align 8
%argslist55531$ae485162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48517, %struct.ScmObj* %argslist55531$ae485161)
store volatile %struct.ScmObj* %argslist55531$ae485162, %struct.ScmObj** %stackaddr$prim55556, align 8
%clofunc55557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48516)
musttail call tailcc void %clofunc55557(%struct.ScmObj* %ae48516, %struct.ScmObj* %argslist55531$ae485162)
ret void
}

define tailcc void @proc_clo$ae48516(%struct.ScmObj* %env$ae48516,%struct.ScmObj* %current_45args54917) {
%stackaddr$env-ref55558 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48516, i64 0)
store %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$env-ref55558
%stackaddr$prim55559 = alloca %struct.ScmObj*, align 8
%_95k48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54917)
store volatile %struct.ScmObj* %_95k48300, %struct.ScmObj** %stackaddr$prim55559, align 8
%stackaddr$prim55560 = alloca %struct.ScmObj*, align 8
%current_45args54918 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54917)
store volatile %struct.ScmObj* %current_45args54918, %struct.ScmObj** %stackaddr$prim55560, align 8
%stackaddr$prim55561 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54918)
store volatile %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$prim55561, align 8
%stackaddr$makeclosure55562 = alloca %struct.ScmObj*, align 8
%fptrToInt55563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48631 to i64
%ae48631 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55563)
store volatile %struct.ScmObj* %ae48631, %struct.ScmObj** %stackaddr$makeclosure55562, align 8
%argslist55510$anf_45bind481640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55564 = alloca %struct.ScmObj*, align 8
%argslist55510$anf_45bind481641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48168, %struct.ScmObj* %argslist55510$anf_45bind481640)
store volatile %struct.ScmObj* %argslist55510$anf_45bind481641, %struct.ScmObj** %stackaddr$prim55564, align 8
%stackaddr$prim55565 = alloca %struct.ScmObj*, align 8
%argslist55510$anf_45bind481642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48631, %struct.ScmObj* %argslist55510$anf_45bind481641)
store volatile %struct.ScmObj* %argslist55510$anf_45bind481642, %struct.ScmObj** %stackaddr$prim55565, align 8
%clofunc55566 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48164)
musttail call tailcc void %clofunc55566(%struct.ScmObj* %anf_45bind48164, %struct.ScmObj* %argslist55510$anf_45bind481642)
ret void
}

define tailcc void @proc_clo$ae48631(%struct.ScmObj* %env$ae48631,%struct.ScmObj* %current_45args54920) {
%stackaddr$prim55567 = alloca %struct.ScmObj*, align 8
%_95k48301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54920)
store volatile %struct.ScmObj* %_95k48301, %struct.ScmObj** %stackaddr$prim55567, align 8
%stackaddr$prim55568 = alloca %struct.ScmObj*, align 8
%current_45args54921 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54920)
store volatile %struct.ScmObj* %current_45args54921, %struct.ScmObj** %stackaddr$prim55568, align 8
%stackaddr$prim55569 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54921)
store volatile %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$prim55569, align 8
%stackaddr$makeclosure55570 = alloca %struct.ScmObj*, align 8
%fptrToInt55571 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48633 to i64
%ae48633 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55571)
store volatile %struct.ScmObj* %ae48633, %struct.ScmObj** %stackaddr$makeclosure55570, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %Ycmb48026, i64 0)
%ae48634 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55572 = alloca %struct.ScmObj*, align 8
%fptrToInt55573 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48635 to i64
%ae48635 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55573)
store volatile %struct.ScmObj* %ae48635, %struct.ScmObj** %stackaddr$makeclosure55572, align 8
%argslist55509$ae486330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55574 = alloca %struct.ScmObj*, align 8
%argslist55509$ae486331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48635, %struct.ScmObj* %argslist55509$ae486330)
store volatile %struct.ScmObj* %argslist55509$ae486331, %struct.ScmObj** %stackaddr$prim55574, align 8
%stackaddr$prim55575 = alloca %struct.ScmObj*, align 8
%argslist55509$ae486332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48634, %struct.ScmObj* %argslist55509$ae486331)
store volatile %struct.ScmObj* %argslist55509$ae486332, %struct.ScmObj** %stackaddr$prim55575, align 8
%clofunc55576 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48633)
musttail call tailcc void %clofunc55576(%struct.ScmObj* %ae48633, %struct.ScmObj* %argslist55509$ae486332)
ret void
}

define tailcc void @proc_clo$ae48633(%struct.ScmObj* %env$ae48633,%struct.ScmObj* %current_45args54923) {
%stackaddr$env-ref55577 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 0)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55577
%stackaddr$prim55578 = alloca %struct.ScmObj*, align 8
%_95k48302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54923)
store volatile %struct.ScmObj* %_95k48302, %struct.ScmObj** %stackaddr$prim55578, align 8
%stackaddr$prim55579 = alloca %struct.ScmObj*, align 8
%current_45args54924 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54923)
store volatile %struct.ScmObj* %current_45args54924, %struct.ScmObj** %stackaddr$prim55579, align 8
%stackaddr$prim55580 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54924)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim55580, align 8
%stackaddr$makeclosure55581 = alloca %struct.ScmObj*, align 8
%fptrToInt55582 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48711 to i64
%ae48711 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55582)
store volatile %struct.ScmObj* %ae48711, %struct.ScmObj** %stackaddr$makeclosure55581, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48711, %struct.ScmObj* %Ycmb48026, i64 0)
%argslist55493$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55583 = alloca %struct.ScmObj*, align 8
%argslist55493$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48173, %struct.ScmObj* %argslist55493$Ycmb480260)
store volatile %struct.ScmObj* %argslist55493$Ycmb480261, %struct.ScmObj** %stackaddr$prim55583, align 8
%stackaddr$prim55584 = alloca %struct.ScmObj*, align 8
%argslist55493$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48711, %struct.ScmObj* %argslist55493$Ycmb480261)
store volatile %struct.ScmObj* %argslist55493$Ycmb480262, %struct.ScmObj** %stackaddr$prim55584, align 8
%clofunc55585 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc55585(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist55493$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae48711(%struct.ScmObj* %env$ae48711,%struct.ScmObj* %current_45args54926) {
%stackaddr$env-ref55586 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48711, i64 0)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55586
%stackaddr$prim55587 = alloca %struct.ScmObj*, align 8
%_95k48303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54926)
store volatile %struct.ScmObj* %_95k48303, %struct.ScmObj** %stackaddr$prim55587, align 8
%stackaddr$prim55588 = alloca %struct.ScmObj*, align 8
%current_45args54927 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54926)
store volatile %struct.ScmObj* %current_45args54927, %struct.ScmObj** %stackaddr$prim55588, align 8
%stackaddr$prim55589 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54927)
store volatile %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$prim55589, align 8
%stackaddr$makeclosure55590 = alloca %struct.ScmObj*, align 8
%fptrToInt55591 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48713 to i64
%ae48713 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55591)
store volatile %struct.ScmObj* %ae48713, %struct.ScmObj** %stackaddr$makeclosure55590, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48713, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48713, %struct.ScmObj* %Ycmb48026, i64 1)
%ae48714 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55592 = alloca %struct.ScmObj*, align 8
%fptrToInt55593 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48715 to i64
%ae48715 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55593)
store volatile %struct.ScmObj* %ae48715, %struct.ScmObj** %stackaddr$makeclosure55592, align 8
%argslist55492$ae487130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55594 = alloca %struct.ScmObj*, align 8
%argslist55492$ae487131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48715, %struct.ScmObj* %argslist55492$ae487130)
store volatile %struct.ScmObj* %argslist55492$ae487131, %struct.ScmObj** %stackaddr$prim55594, align 8
%stackaddr$prim55595 = alloca %struct.ScmObj*, align 8
%argslist55492$ae487132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48714, %struct.ScmObj* %argslist55492$ae487131)
store volatile %struct.ScmObj* %argslist55492$ae487132, %struct.ScmObj** %stackaddr$prim55595, align 8
%clofunc55596 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48713)
musttail call tailcc void %clofunc55596(%struct.ScmObj* %ae48713, %struct.ScmObj* %argslist55492$ae487132)
ret void
}

define tailcc void @proc_clo$ae48713(%struct.ScmObj* %env$ae48713,%struct.ScmObj* %current_45args54929) {
%stackaddr$env-ref55597 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48713, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55597
%stackaddr$env-ref55598 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48713, i64 1)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55598
%stackaddr$prim55599 = alloca %struct.ScmObj*, align 8
%_95k48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54929)
store volatile %struct.ScmObj* %_95k48304, %struct.ScmObj** %stackaddr$prim55599, align 8
%stackaddr$prim55600 = alloca %struct.ScmObj*, align 8
%current_45args54930 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54929)
store volatile %struct.ScmObj* %current_45args54930, %struct.ScmObj** %stackaddr$prim55600, align 8
%stackaddr$prim55601 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54930)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim55601, align 8
%stackaddr$makeclosure55602 = alloca %struct.ScmObj*, align 8
%fptrToInt55603 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48808 to i64
%ae48808 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55603)
store volatile %struct.ScmObj* %ae48808, %struct.ScmObj** %stackaddr$makeclosure55602, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48808, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48808, %struct.ScmObj* %Ycmb48026, i64 1)
%argslist55473$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55604 = alloca %struct.ScmObj*, align 8
%argslist55473$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48179, %struct.ScmObj* %argslist55473$Ycmb480260)
store volatile %struct.ScmObj* %argslist55473$Ycmb480261, %struct.ScmObj** %stackaddr$prim55604, align 8
%stackaddr$prim55605 = alloca %struct.ScmObj*, align 8
%argslist55473$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48808, %struct.ScmObj* %argslist55473$Ycmb480261)
store volatile %struct.ScmObj* %argslist55473$Ycmb480262, %struct.ScmObj** %stackaddr$prim55605, align 8
%clofunc55606 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc55606(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist55473$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae48808(%struct.ScmObj* %env$ae48808,%struct.ScmObj* %current_45args54932) {
%stackaddr$env-ref55607 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48808, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55607
%stackaddr$env-ref55608 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48808, i64 1)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55608
%stackaddr$prim55609 = alloca %struct.ScmObj*, align 8
%_95k48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54932)
store volatile %struct.ScmObj* %_95k48305, %struct.ScmObj** %stackaddr$prim55609, align 8
%stackaddr$prim55610 = alloca %struct.ScmObj*, align 8
%current_45args54933 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54932)
store volatile %struct.ScmObj* %current_45args54933, %struct.ScmObj** %stackaddr$prim55610, align 8
%stackaddr$prim55611 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54933)
store volatile %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$prim55611, align 8
%stackaddr$makeclosure55612 = alloca %struct.ScmObj*, align 8
%fptrToInt55613 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48810 to i64
%ae48810 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55613)
store volatile %struct.ScmObj* %ae48810, %struct.ScmObj** %stackaddr$makeclosure55612, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48810, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48810, %struct.ScmObj* %_37map148043, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48810, %struct.ScmObj* %Ycmb48026, i64 2)
%ae48811 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55614 = alloca %struct.ScmObj*, align 8
%fptrToInt55615 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48812 to i64
%ae48812 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55615)
store volatile %struct.ScmObj* %ae48812, %struct.ScmObj** %stackaddr$makeclosure55614, align 8
%argslist55472$ae488100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55616 = alloca %struct.ScmObj*, align 8
%argslist55472$ae488101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48812, %struct.ScmObj* %argslist55472$ae488100)
store volatile %struct.ScmObj* %argslist55472$ae488101, %struct.ScmObj** %stackaddr$prim55616, align 8
%stackaddr$prim55617 = alloca %struct.ScmObj*, align 8
%argslist55472$ae488102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48811, %struct.ScmObj* %argslist55472$ae488101)
store volatile %struct.ScmObj* %argslist55472$ae488102, %struct.ScmObj** %stackaddr$prim55617, align 8
%clofunc55618 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48810)
musttail call tailcc void %clofunc55618(%struct.ScmObj* %ae48810, %struct.ScmObj* %argslist55472$ae488102)
ret void
}

define tailcc void @proc_clo$ae48810(%struct.ScmObj* %env$ae48810,%struct.ScmObj* %current_45args54935) {
%stackaddr$env-ref55619 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48810, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55619
%stackaddr$env-ref55620 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48810, i64 1)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55620
%stackaddr$env-ref55621 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48810, i64 2)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55621
%stackaddr$prim55622 = alloca %struct.ScmObj*, align 8
%_95k48306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54935)
store volatile %struct.ScmObj* %_95k48306, %struct.ScmObj** %stackaddr$prim55622, align 8
%stackaddr$prim55623 = alloca %struct.ScmObj*, align 8
%current_45args54936 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54935)
store volatile %struct.ScmObj* %current_45args54936, %struct.ScmObj** %stackaddr$prim55623, align 8
%stackaddr$prim55624 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54936)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim55624, align 8
%stackaddr$makeclosure55625 = alloca %struct.ScmObj*, align 8
%fptrToInt55626 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48958 to i64
%ae48958 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55626)
store volatile %struct.ScmObj* %ae48958, %struct.ScmObj** %stackaddr$makeclosure55625, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48958, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48958, %struct.ScmObj* %_37map148043, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48958, %struct.ScmObj* %Ycmb48026, i64 2)
%argslist55456$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55627 = alloca %struct.ScmObj*, align 8
%argslist55456$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48186, %struct.ScmObj* %argslist55456$Ycmb480260)
store volatile %struct.ScmObj* %argslist55456$Ycmb480261, %struct.ScmObj** %stackaddr$prim55627, align 8
%stackaddr$prim55628 = alloca %struct.ScmObj*, align 8
%argslist55456$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48958, %struct.ScmObj* %argslist55456$Ycmb480261)
store volatile %struct.ScmObj* %argslist55456$Ycmb480262, %struct.ScmObj** %stackaddr$prim55628, align 8
%clofunc55629 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc55629(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist55456$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae48958(%struct.ScmObj* %env$ae48958,%struct.ScmObj* %current_45args54938) {
%stackaddr$env-ref55630 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48958, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55630
%stackaddr$env-ref55631 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48958, i64 1)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55631
%stackaddr$env-ref55632 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48958, i64 2)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55632
%stackaddr$prim55633 = alloca %struct.ScmObj*, align 8
%_95k48307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54938)
store volatile %struct.ScmObj* %_95k48307, %struct.ScmObj** %stackaddr$prim55633, align 8
%stackaddr$prim55634 = alloca %struct.ScmObj*, align 8
%current_45args54939 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54938)
store volatile %struct.ScmObj* %current_45args54939, %struct.ScmObj** %stackaddr$prim55634, align 8
%stackaddr$prim55635 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54939)
store volatile %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$prim55635, align 8
%stackaddr$makeclosure55636 = alloca %struct.ScmObj*, align 8
%fptrToInt55637 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48960 to i64
%ae48960 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55637)
store volatile %struct.ScmObj* %ae48960, %struct.ScmObj** %stackaddr$makeclosure55636, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48960, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48960, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48960, %struct.ScmObj* %_37map148043, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48960, %struct.ScmObj* %Ycmb48026, i64 3)
%ae48961 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55638 = alloca %struct.ScmObj*, align 8
%fptrToInt55639 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48962 to i64
%ae48962 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55639)
store volatile %struct.ScmObj* %ae48962, %struct.ScmObj** %stackaddr$makeclosure55638, align 8
%argslist55455$ae489600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55640 = alloca %struct.ScmObj*, align 8
%argslist55455$ae489601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48962, %struct.ScmObj* %argslist55455$ae489600)
store volatile %struct.ScmObj* %argslist55455$ae489601, %struct.ScmObj** %stackaddr$prim55640, align 8
%stackaddr$prim55641 = alloca %struct.ScmObj*, align 8
%argslist55455$ae489602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48961, %struct.ScmObj* %argslist55455$ae489601)
store volatile %struct.ScmObj* %argslist55455$ae489602, %struct.ScmObj** %stackaddr$prim55641, align 8
%clofunc55642 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48960)
musttail call tailcc void %clofunc55642(%struct.ScmObj* %ae48960, %struct.ScmObj* %argslist55455$ae489602)
ret void
}

define tailcc void @proc_clo$ae48960(%struct.ScmObj* %env$ae48960,%struct.ScmObj* %current_45args54941) {
%stackaddr$env-ref55643 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48960, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref55643
%stackaddr$env-ref55644 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48960, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55644
%stackaddr$env-ref55645 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48960, i64 2)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55645
%stackaddr$env-ref55646 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48960, i64 3)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55646
%stackaddr$prim55647 = alloca %struct.ScmObj*, align 8
%_95k48308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54941)
store volatile %struct.ScmObj* %_95k48308, %struct.ScmObj** %stackaddr$prim55647, align 8
%stackaddr$prim55648 = alloca %struct.ScmObj*, align 8
%current_45args54942 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54941)
store volatile %struct.ScmObj* %current_45args54942, %struct.ScmObj** %stackaddr$prim55648, align 8
%stackaddr$prim55649 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54942)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim55649, align 8
%stackaddr$makeclosure55650 = alloca %struct.ScmObj*, align 8
%fptrToInt55651 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49041 to i64
%ae49041 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55651)
store volatile %struct.ScmObj* %ae49041, %struct.ScmObj** %stackaddr$makeclosure55650, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49041, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49041, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49041, %struct.ScmObj* %_37map148043, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49041, %struct.ScmObj* %Ycmb48026, i64 3)
%argslist55441$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55652 = alloca %struct.ScmObj*, align 8
%argslist55441$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48190, %struct.ScmObj* %argslist55441$Ycmb480260)
store volatile %struct.ScmObj* %argslist55441$Ycmb480261, %struct.ScmObj** %stackaddr$prim55652, align 8
%stackaddr$prim55653 = alloca %struct.ScmObj*, align 8
%argslist55441$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49041, %struct.ScmObj* %argslist55441$Ycmb480261)
store volatile %struct.ScmObj* %argslist55441$Ycmb480262, %struct.ScmObj** %stackaddr$prim55653, align 8
%clofunc55654 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc55654(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist55441$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae49041(%struct.ScmObj* %env$ae49041,%struct.ScmObj* %current_45args54944) {
%stackaddr$env-ref55655 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49041, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref55655
%stackaddr$env-ref55656 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49041, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55656
%stackaddr$env-ref55657 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49041, i64 2)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55657
%stackaddr$env-ref55658 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49041, i64 3)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55658
%stackaddr$prim55659 = alloca %struct.ScmObj*, align 8
%_95k48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54944)
store volatile %struct.ScmObj* %_95k48309, %struct.ScmObj** %stackaddr$prim55659, align 8
%stackaddr$prim55660 = alloca %struct.ScmObj*, align 8
%current_45args54945 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54944)
store volatile %struct.ScmObj* %current_45args54945, %struct.ScmObj** %stackaddr$prim55660, align 8
%stackaddr$prim55661 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54945)
store volatile %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$prim55661, align 8
%stackaddr$makeclosure55662 = alloca %struct.ScmObj*, align 8
%fptrToInt55663 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49043 to i64
%ae49043 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55663)
store volatile %struct.ScmObj* %ae49043, %struct.ScmObj** %stackaddr$makeclosure55662, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %_37length48036, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %_37foldr148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %_37map148043, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %Ycmb48026, i64 4)
%ae49044 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55664 = alloca %struct.ScmObj*, align 8
%fptrToInt55665 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49045 to i64
%ae49045 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55665)
store volatile %struct.ScmObj* %ae49045, %struct.ScmObj** %stackaddr$makeclosure55664, align 8
%argslist55440$ae490430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55666 = alloca %struct.ScmObj*, align 8
%argslist55440$ae490431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49045, %struct.ScmObj* %argslist55440$ae490430)
store volatile %struct.ScmObj* %argslist55440$ae490431, %struct.ScmObj** %stackaddr$prim55666, align 8
%stackaddr$prim55667 = alloca %struct.ScmObj*, align 8
%argslist55440$ae490432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49044, %struct.ScmObj* %argslist55440$ae490431)
store volatile %struct.ScmObj* %argslist55440$ae490432, %struct.ScmObj** %stackaddr$prim55667, align 8
%clofunc55668 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49043)
musttail call tailcc void %clofunc55668(%struct.ScmObj* %ae49043, %struct.ScmObj* %argslist55440$ae490432)
ret void
}

define tailcc void @proc_clo$ae49043(%struct.ScmObj* %env$ae49043,%struct.ScmObj* %current_45args54947) {
%stackaddr$env-ref55669 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref55669
%stackaddr$env-ref55670 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 1)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref55670
%stackaddr$env-ref55671 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 2)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55671
%stackaddr$env-ref55672 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 3)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55672
%stackaddr$env-ref55673 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55673
%stackaddr$prim55674 = alloca %struct.ScmObj*, align 8
%_95k48310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54947)
store volatile %struct.ScmObj* %_95k48310, %struct.ScmObj** %stackaddr$prim55674, align 8
%stackaddr$prim55675 = alloca %struct.ScmObj*, align 8
%current_45args54948 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54947)
store volatile %struct.ScmObj* %current_45args54948, %struct.ScmObj** %stackaddr$prim55675, align 8
%stackaddr$prim55676 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54948)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim55676, align 8
%stackaddr$makeclosure55677 = alloca %struct.ScmObj*, align 8
%fptrToInt55678 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49120 to i64
%ae49120 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55678)
store volatile %struct.ScmObj* %ae49120, %struct.ScmObj** %stackaddr$makeclosure55677, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49120, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49120, %struct.ScmObj* %_37length48036, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49120, %struct.ScmObj* %_37foldr148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49120, %struct.ScmObj* %_37map148043, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49120, %struct.ScmObj* %Ycmb48026, i64 4)
%argslist55424$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55679 = alloca %struct.ScmObj*, align 8
%argslist55424$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48195, %struct.ScmObj* %argslist55424$Ycmb480260)
store volatile %struct.ScmObj* %argslist55424$Ycmb480261, %struct.ScmObj** %stackaddr$prim55679, align 8
%stackaddr$prim55680 = alloca %struct.ScmObj*, align 8
%argslist55424$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49120, %struct.ScmObj* %argslist55424$Ycmb480261)
store volatile %struct.ScmObj* %argslist55424$Ycmb480262, %struct.ScmObj** %stackaddr$prim55680, align 8
%clofunc55681 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc55681(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist55424$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae49120(%struct.ScmObj* %env$ae49120,%struct.ScmObj* %current_45args54950) {
%stackaddr$env-ref55682 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49120, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref55682
%stackaddr$env-ref55683 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49120, i64 1)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref55683
%stackaddr$env-ref55684 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49120, i64 2)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55684
%stackaddr$env-ref55685 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49120, i64 3)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55685
%stackaddr$env-ref55686 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49120, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55686
%stackaddr$prim55687 = alloca %struct.ScmObj*, align 8
%_95k48311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54950)
store volatile %struct.ScmObj* %_95k48311, %struct.ScmObj** %stackaddr$prim55687, align 8
%stackaddr$prim55688 = alloca %struct.ScmObj*, align 8
%current_45args54951 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54950)
store volatile %struct.ScmObj* %current_45args54951, %struct.ScmObj** %stackaddr$prim55688, align 8
%stackaddr$prim55689 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54951)
store volatile %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$prim55689, align 8
%stackaddr$makeclosure55690 = alloca %struct.ScmObj*, align 8
%fptrToInt55691 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49122 to i64
%ae49122 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55691)
store volatile %struct.ScmObj* %ae49122, %struct.ScmObj** %stackaddr$makeclosure55690, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49122, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49122, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49122, %struct.ScmObj* %_37take48039, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49122, %struct.ScmObj* %_37length48036, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49122, %struct.ScmObj* %_37map148043, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49122, %struct.ScmObj* %Ycmb48026, i64 5)
%ae49123 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55692 = alloca %struct.ScmObj*, align 8
%fptrToInt55693 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49124 to i64
%ae49124 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55693)
store volatile %struct.ScmObj* %ae49124, %struct.ScmObj** %stackaddr$makeclosure55692, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49124, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist55423$ae491220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55694 = alloca %struct.ScmObj*, align 8
%argslist55423$ae491221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49124, %struct.ScmObj* %argslist55423$ae491220)
store volatile %struct.ScmObj* %argslist55423$ae491221, %struct.ScmObj** %stackaddr$prim55694, align 8
%stackaddr$prim55695 = alloca %struct.ScmObj*, align 8
%argslist55423$ae491222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49123, %struct.ScmObj* %argslist55423$ae491221)
store volatile %struct.ScmObj* %argslist55423$ae491222, %struct.ScmObj** %stackaddr$prim55695, align 8
%clofunc55696 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49122)
musttail call tailcc void %clofunc55696(%struct.ScmObj* %ae49122, %struct.ScmObj* %argslist55423$ae491222)
ret void
}

define tailcc void @proc_clo$ae49122(%struct.ScmObj* %env$ae49122,%struct.ScmObj* %current_45args54953) {
%stackaddr$env-ref55697 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49122, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55697
%stackaddr$env-ref55698 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49122, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55698
%stackaddr$env-ref55699 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49122, i64 2)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref55699
%stackaddr$env-ref55700 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49122, i64 3)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref55700
%stackaddr$env-ref55701 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49122, i64 4)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55701
%stackaddr$env-ref55702 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49122, i64 5)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55702
%stackaddr$prim55703 = alloca %struct.ScmObj*, align 8
%_95k48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54953)
store volatile %struct.ScmObj* %_95k48312, %struct.ScmObj** %stackaddr$prim55703, align 8
%stackaddr$prim55704 = alloca %struct.ScmObj*, align 8
%current_45args54954 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54953)
store volatile %struct.ScmObj* %current_45args54954, %struct.ScmObj** %stackaddr$prim55704, align 8
%stackaddr$prim55705 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54954)
store volatile %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$prim55705, align 8
%stackaddr$makeclosure55706 = alloca %struct.ScmObj*, align 8
%fptrToInt55707 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49176 to i64
%ae49176 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55707)
store volatile %struct.ScmObj* %ae49176, %struct.ScmObj** %stackaddr$makeclosure55706, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49176, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49176, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49176, %struct.ScmObj* %_37last48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49176, %struct.ScmObj* %_37map148043, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49176, %struct.ScmObj* %Ycmb48026, i64 4)
%ae49177 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55708 = alloca %struct.ScmObj*, align 8
%fptrToInt55709 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49178 to i64
%ae49178 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55709)
store volatile %struct.ScmObj* %ae49178, %struct.ScmObj** %stackaddr$makeclosure55708, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49178, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49178, %struct.ScmObj* %_37length48036, i64 1)
%argslist55409$ae491760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55710 = alloca %struct.ScmObj*, align 8
%argslist55409$ae491761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49178, %struct.ScmObj* %argslist55409$ae491760)
store volatile %struct.ScmObj* %argslist55409$ae491761, %struct.ScmObj** %stackaddr$prim55710, align 8
%stackaddr$prim55711 = alloca %struct.ScmObj*, align 8
%argslist55409$ae491762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49177, %struct.ScmObj* %argslist55409$ae491761)
store volatile %struct.ScmObj* %argslist55409$ae491762, %struct.ScmObj** %stackaddr$prim55711, align 8
%clofunc55712 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49176)
musttail call tailcc void %clofunc55712(%struct.ScmObj* %ae49176, %struct.ScmObj* %argslist55409$ae491762)
ret void
}

define tailcc void @proc_clo$ae49176(%struct.ScmObj* %env$ae49176,%struct.ScmObj* %current_45args54956) {
%stackaddr$env-ref55713 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49176, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55713
%stackaddr$env-ref55714 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49176, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55714
%stackaddr$env-ref55715 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49176, i64 2)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref55715
%stackaddr$env-ref55716 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49176, i64 3)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55716
%stackaddr$env-ref55717 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49176, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55717
%stackaddr$prim55718 = alloca %struct.ScmObj*, align 8
%_95k48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54956)
store volatile %struct.ScmObj* %_95k48313, %struct.ScmObj** %stackaddr$prim55718, align 8
%stackaddr$prim55719 = alloca %struct.ScmObj*, align 8
%current_45args54957 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54956)
store volatile %struct.ScmObj* %current_45args54957, %struct.ScmObj** %stackaddr$prim55719, align 8
%stackaddr$prim55720 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54957)
store volatile %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$prim55720, align 8
%stackaddr$makeclosure55721 = alloca %struct.ScmObj*, align 8
%fptrToInt55722 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49206 to i64
%ae49206 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55722)
store volatile %struct.ScmObj* %ae49206, %struct.ScmObj** %stackaddr$makeclosure55721, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49206, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49206, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49206, %struct.ScmObj* %_37last48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49206, %struct.ScmObj* %_37drop_45right48066, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49206, %struct.ScmObj* %Ycmb48026, i64 4)
%ae49207 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55723 = alloca %struct.ScmObj*, align 8
%fptrToInt55724 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49208 to i64
%ae49208 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55724)
store volatile %struct.ScmObj* %ae49208, %struct.ScmObj** %stackaddr$makeclosure55723, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49208, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49208, %struct.ScmObj* %_37map148043, i64 1)
%argslist55399$ae492060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55725 = alloca %struct.ScmObj*, align 8
%argslist55399$ae492061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49208, %struct.ScmObj* %argslist55399$ae492060)
store volatile %struct.ScmObj* %argslist55399$ae492061, %struct.ScmObj** %stackaddr$prim55725, align 8
%stackaddr$prim55726 = alloca %struct.ScmObj*, align 8
%argslist55399$ae492062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49207, %struct.ScmObj* %argslist55399$ae492061)
store volatile %struct.ScmObj* %argslist55399$ae492062, %struct.ScmObj** %stackaddr$prim55726, align 8
%clofunc55727 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49206)
musttail call tailcc void %clofunc55727(%struct.ScmObj* %ae49206, %struct.ScmObj* %argslist55399$ae492062)
ret void
}

define tailcc void @proc_clo$ae49206(%struct.ScmObj* %env$ae49206,%struct.ScmObj* %current_45args54959) {
%stackaddr$env-ref55728 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49206, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55728
%stackaddr$env-ref55729 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49206, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55729
%stackaddr$env-ref55730 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49206, i64 2)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref55730
%stackaddr$env-ref55731 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49206, i64 3)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref55731
%stackaddr$env-ref55732 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49206, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55732
%stackaddr$prim55733 = alloca %struct.ScmObj*, align 8
%_95k48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54959)
store volatile %struct.ScmObj* %_95k48314, %struct.ScmObj** %stackaddr$prim55733, align 8
%stackaddr$prim55734 = alloca %struct.ScmObj*, align 8
%current_45args54960 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54959)
store volatile %struct.ScmObj* %current_45args54960, %struct.ScmObj** %stackaddr$prim55734, align 8
%stackaddr$prim55735 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54960)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim55735, align 8
%stackaddr$makeclosure55736 = alloca %struct.ScmObj*, align 8
%fptrToInt55737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49590 to i64
%ae49590 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55737)
store volatile %struct.ScmObj* %ae49590, %struct.ScmObj** %stackaddr$makeclosure55736, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49590, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49590, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49590, %struct.ScmObj* %_37last48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49590, %struct.ScmObj* %_37drop_45right48066, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49590, %struct.ScmObj* %Ycmb48026, i64 4)
%argslist55339$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55738 = alloca %struct.ScmObj*, align 8
%argslist55339$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %argslist55339$Ycmb480260)
store volatile %struct.ScmObj* %argslist55339$Ycmb480261, %struct.ScmObj** %stackaddr$prim55738, align 8
%stackaddr$prim55739 = alloca %struct.ScmObj*, align 8
%argslist55339$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49590, %struct.ScmObj* %argslist55339$Ycmb480261)
store volatile %struct.ScmObj* %argslist55339$Ycmb480262, %struct.ScmObj** %stackaddr$prim55739, align 8
%clofunc55740 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc55740(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist55339$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae49590(%struct.ScmObj* %env$ae49590,%struct.ScmObj* %current_45args54962) {
%stackaddr$env-ref55741 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49590, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55741
%stackaddr$env-ref55742 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49590, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55742
%stackaddr$env-ref55743 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49590, i64 2)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref55743
%stackaddr$env-ref55744 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49590, i64 3)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref55744
%stackaddr$env-ref55745 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49590, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55745
%stackaddr$prim55746 = alloca %struct.ScmObj*, align 8
%_95k48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54962)
store volatile %struct.ScmObj* %_95k48315, %struct.ScmObj** %stackaddr$prim55746, align 8
%stackaddr$prim55747 = alloca %struct.ScmObj*, align 8
%current_45args54963 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54962)
store volatile %struct.ScmObj* %current_45args54963, %struct.ScmObj** %stackaddr$prim55747, align 8
%stackaddr$prim55748 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54963)
store volatile %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$prim55748, align 8
%stackaddr$makeclosure55749 = alloca %struct.ScmObj*, align 8
%fptrToInt55750 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49592 to i64
%ae49592 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55750)
store volatile %struct.ScmObj* %ae49592, %struct.ScmObj** %stackaddr$makeclosure55749, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49592, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49592, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49592, %struct.ScmObj* %_37last48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49592, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49592, %struct.ScmObj* %_37drop_45right48066, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49592, %struct.ScmObj* %Ycmb48026, i64 5)
%ae49593 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55751 = alloca %struct.ScmObj*, align 8
%fptrToInt55752 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49594 to i64
%ae49594 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55752)
store volatile %struct.ScmObj* %ae49594, %struct.ScmObj** %stackaddr$makeclosure55751, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49594, %struct.ScmObj* %_37foldr148047, i64 0)
%argslist55338$ae495920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55753 = alloca %struct.ScmObj*, align 8
%argslist55338$ae495921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49594, %struct.ScmObj* %argslist55338$ae495920)
store volatile %struct.ScmObj* %argslist55338$ae495921, %struct.ScmObj** %stackaddr$prim55753, align 8
%stackaddr$prim55754 = alloca %struct.ScmObj*, align 8
%argslist55338$ae495922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49593, %struct.ScmObj* %argslist55338$ae495921)
store volatile %struct.ScmObj* %argslist55338$ae495922, %struct.ScmObj** %stackaddr$prim55754, align 8
%clofunc55755 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49592)
musttail call tailcc void %clofunc55755(%struct.ScmObj* %ae49592, %struct.ScmObj* %argslist55338$ae495922)
ret void
}

define tailcc void @proc_clo$ae49592(%struct.ScmObj* %env$ae49592,%struct.ScmObj* %current_45args54965) {
%stackaddr$env-ref55756 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49592, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55756
%stackaddr$env-ref55757 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49592, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55757
%stackaddr$env-ref55758 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49592, i64 2)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref55758
%stackaddr$env-ref55759 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49592, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55759
%stackaddr$env-ref55760 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49592, i64 4)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref55760
%stackaddr$env-ref55761 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49592, i64 5)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55761
%stackaddr$prim55762 = alloca %struct.ScmObj*, align 8
%_95k48316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54965)
store volatile %struct.ScmObj* %_95k48316, %struct.ScmObj** %stackaddr$prim55762, align 8
%stackaddr$prim55763 = alloca %struct.ScmObj*, align 8
%current_45args54966 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54965)
store volatile %struct.ScmObj* %current_45args54966, %struct.ScmObj** %stackaddr$prim55763, align 8
%stackaddr$prim55764 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54966)
store volatile %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$prim55764, align 8
%stackaddr$makeclosure55765 = alloca %struct.ScmObj*, align 8
%fptrToInt55766 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49669 to i64
%ae49669 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55766)
store volatile %struct.ScmObj* %ae49669, %struct.ScmObj** %stackaddr$makeclosure55765, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49669, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49669, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49669, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49669, %struct.ScmObj* %_37map148078, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49669, %struct.ScmObj* %Ycmb48026, i64 4)
%ae49670 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55767 = alloca %struct.ScmObj*, align 8
%fptrToInt55768 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49671 to i64
%ae49671 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55768)
store volatile %struct.ScmObj* %ae49671, %struct.ScmObj** %stackaddr$makeclosure55767, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49671, %struct.ScmObj* %_37last48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49671, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49671, %struct.ScmObj* %_37drop_45right48066, i64 2)
%argslist55319$ae496690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55769 = alloca %struct.ScmObj*, align 8
%argslist55319$ae496691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49671, %struct.ScmObj* %argslist55319$ae496690)
store volatile %struct.ScmObj* %argslist55319$ae496691, %struct.ScmObj** %stackaddr$prim55769, align 8
%stackaddr$prim55770 = alloca %struct.ScmObj*, align 8
%argslist55319$ae496692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49670, %struct.ScmObj* %argslist55319$ae496691)
store volatile %struct.ScmObj* %argslist55319$ae496692, %struct.ScmObj** %stackaddr$prim55770, align 8
%clofunc55771 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49669)
musttail call tailcc void %clofunc55771(%struct.ScmObj* %ae49669, %struct.ScmObj* %argslist55319$ae496692)
ret void
}

define tailcc void @proc_clo$ae49669(%struct.ScmObj* %env$ae49669,%struct.ScmObj* %current_45args54968) {
%stackaddr$env-ref55772 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49669, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55772
%stackaddr$env-ref55773 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49669, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55773
%stackaddr$env-ref55774 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49669, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55774
%stackaddr$env-ref55775 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49669, i64 3)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55775
%stackaddr$env-ref55776 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49669, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55776
%stackaddr$prim55777 = alloca %struct.ScmObj*, align 8
%_95k48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54968)
store volatile %struct.ScmObj* %_95k48317, %struct.ScmObj** %stackaddr$prim55777, align 8
%stackaddr$prim55778 = alloca %struct.ScmObj*, align 8
%current_45args54969 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54968)
store volatile %struct.ScmObj* %current_45args54969, %struct.ScmObj** %stackaddr$prim55778, align 8
%stackaddr$prim55779 = alloca %struct.ScmObj*, align 8
%_37map48073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54969)
store volatile %struct.ScmObj* %_37map48073, %struct.ScmObj** %stackaddr$prim55779, align 8
%stackaddr$makeclosure55780 = alloca %struct.ScmObj*, align 8
%fptrToInt55781 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49815 to i64
%ae49815 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55781)
store volatile %struct.ScmObj* %ae49815, %struct.ScmObj** %stackaddr$makeclosure55780, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49815, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49815, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49815, %struct.ScmObj* %Ycmb48026, i64 2)
%ae49816 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55782 = alloca %struct.ScmObj*, align 8
%fptrToInt55783 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49817 to i64
%ae49817 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55783)
store volatile %struct.ScmObj* %ae49817, %struct.ScmObj** %stackaddr$makeclosure55782, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49817, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49817, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49817, %struct.ScmObj* %_37map148078, i64 2)
%argslist55302$ae498150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55784 = alloca %struct.ScmObj*, align 8
%argslist55302$ae498151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49817, %struct.ScmObj* %argslist55302$ae498150)
store volatile %struct.ScmObj* %argslist55302$ae498151, %struct.ScmObj** %stackaddr$prim55784, align 8
%stackaddr$prim55785 = alloca %struct.ScmObj*, align 8
%argslist55302$ae498152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49816, %struct.ScmObj* %argslist55302$ae498151)
store volatile %struct.ScmObj* %argslist55302$ae498152, %struct.ScmObj** %stackaddr$prim55785, align 8
%clofunc55786 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49815)
musttail call tailcc void %clofunc55786(%struct.ScmObj* %ae49815, %struct.ScmObj* %argslist55302$ae498152)
ret void
}

define tailcc void @proc_clo$ae49815(%struct.ScmObj* %env$ae49815,%struct.ScmObj* %current_45args54971) {
%stackaddr$env-ref55787 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49815, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55787
%stackaddr$env-ref55788 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49815, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55788
%stackaddr$env-ref55789 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49815, i64 2)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref55789
%stackaddr$prim55790 = alloca %struct.ScmObj*, align 8
%_95k48318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54971)
store volatile %struct.ScmObj* %_95k48318, %struct.ScmObj** %stackaddr$prim55790, align 8
%stackaddr$prim55791 = alloca %struct.ScmObj*, align 8
%current_45args54972 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54971)
store volatile %struct.ScmObj* %current_45args54972, %struct.ScmObj** %stackaddr$prim55791, align 8
%stackaddr$prim55792 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54972)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55792, align 8
%stackaddr$makeclosure55793 = alloca %struct.ScmObj*, align 8
%fptrToInt55794 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50207 to i64
%ae50207 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55794)
store volatile %struct.ScmObj* %ae50207, %struct.ScmObj** %stackaddr$makeclosure55793, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50207, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50207, %struct.ScmObj* %_37foldl148031, i64 1)
%argslist55242$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55795 = alloca %struct.ScmObj*, align 8
%argslist55242$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48231, %struct.ScmObj* %argslist55242$Ycmb480260)
store volatile %struct.ScmObj* %argslist55242$Ycmb480261, %struct.ScmObj** %stackaddr$prim55795, align 8
%stackaddr$prim55796 = alloca %struct.ScmObj*, align 8
%argslist55242$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50207, %struct.ScmObj* %argslist55242$Ycmb480261)
store volatile %struct.ScmObj* %argslist55242$Ycmb480262, %struct.ScmObj** %stackaddr$prim55796, align 8
%clofunc55797 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc55797(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist55242$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae50207(%struct.ScmObj* %env$ae50207,%struct.ScmObj* %current_45args54974) {
%stackaddr$env-ref55798 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50207, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55798
%stackaddr$env-ref55799 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50207, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55799
%stackaddr$prim55800 = alloca %struct.ScmObj*, align 8
%_95k48319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54974)
store volatile %struct.ScmObj* %_95k48319, %struct.ScmObj** %stackaddr$prim55800, align 8
%stackaddr$prim55801 = alloca %struct.ScmObj*, align 8
%current_45args54975 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54974)
store volatile %struct.ScmObj* %current_45args54975, %struct.ScmObj** %stackaddr$prim55801, align 8
%stackaddr$prim55802 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54975)
store volatile %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$prim55802, align 8
%stackaddr$makeclosure55803 = alloca %struct.ScmObj*, align 8
%fptrToInt55804 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50209 to i64
%ae50209 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55804)
store volatile %struct.ScmObj* %ae50209, %struct.ScmObj** %stackaddr$makeclosure55803, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50209, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50209, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50209, %struct.ScmObj* %_37foldl48129, i64 2)
%ae50210 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55805 = alloca %struct.ScmObj*, align 8
%fptrToInt55806 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50211 to i64
%ae50211 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55806)
store volatile %struct.ScmObj* %ae50211, %struct.ScmObj** %stackaddr$makeclosure55805, align 8
%argslist55241$ae502090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55807 = alloca %struct.ScmObj*, align 8
%argslist55241$ae502091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50211, %struct.ScmObj* %argslist55241$ae502090)
store volatile %struct.ScmObj* %argslist55241$ae502091, %struct.ScmObj** %stackaddr$prim55807, align 8
%stackaddr$prim55808 = alloca %struct.ScmObj*, align 8
%argslist55241$ae502092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50210, %struct.ScmObj* %argslist55241$ae502091)
store volatile %struct.ScmObj* %argslist55241$ae502092, %struct.ScmObj** %stackaddr$prim55808, align 8
%clofunc55809 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50209)
musttail call tailcc void %clofunc55809(%struct.ScmObj* %ae50209, %struct.ScmObj* %argslist55241$ae502092)
ret void
}

define tailcc void @proc_clo$ae50209(%struct.ScmObj* %env$ae50209,%struct.ScmObj* %current_45args54977) {
%stackaddr$env-ref55810 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50209, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55810
%stackaddr$env-ref55811 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50209, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55811
%stackaddr$env-ref55812 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50209, i64 2)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55812
%stackaddr$prim55813 = alloca %struct.ScmObj*, align 8
%_95k48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54977)
store volatile %struct.ScmObj* %_95k48320, %struct.ScmObj** %stackaddr$prim55813, align 8
%stackaddr$prim55814 = alloca %struct.ScmObj*, align 8
%current_45args54978 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54977)
store volatile %struct.ScmObj* %current_45args54978, %struct.ScmObj** %stackaddr$prim55814, align 8
%stackaddr$prim55815 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54978)
store volatile %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$prim55815, align 8
%stackaddr$makeclosure55816 = alloca %struct.ScmObj*, align 8
%fptrToInt55817 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50233 to i64
%ae50233 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55817)
store volatile %struct.ScmObj* %ae50233, %struct.ScmObj** %stackaddr$makeclosure55816, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50233, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50233, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50233, %struct.ScmObj* %_37foldl48129, i64 2)
%ae50234 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55818 = alloca %struct.ScmObj*, align 8
%fptrToInt55819 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50235 to i64
%ae50235 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55819)
store volatile %struct.ScmObj* %ae50235, %struct.ScmObj** %stackaddr$makeclosure55818, align 8
%argslist55235$ae502330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55820 = alloca %struct.ScmObj*, align 8
%argslist55235$ae502331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50235, %struct.ScmObj* %argslist55235$ae502330)
store volatile %struct.ScmObj* %argslist55235$ae502331, %struct.ScmObj** %stackaddr$prim55820, align 8
%stackaddr$prim55821 = alloca %struct.ScmObj*, align 8
%argslist55235$ae502332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50234, %struct.ScmObj* %argslist55235$ae502331)
store volatile %struct.ScmObj* %argslist55235$ae502332, %struct.ScmObj** %stackaddr$prim55821, align 8
%clofunc55822 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50233)
musttail call tailcc void %clofunc55822(%struct.ScmObj* %ae50233, %struct.ScmObj* %argslist55235$ae502332)
ret void
}

define tailcc void @proc_clo$ae50233(%struct.ScmObj* %env$ae50233,%struct.ScmObj* %current_45args54980) {
%stackaddr$env-ref55823 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50233, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55823
%stackaddr$env-ref55824 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50233, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55824
%stackaddr$env-ref55825 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50233, i64 2)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55825
%stackaddr$prim55826 = alloca %struct.ScmObj*, align 8
%_95k48321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54980)
store volatile %struct.ScmObj* %_95k48321, %struct.ScmObj** %stackaddr$prim55826, align 8
%stackaddr$prim55827 = alloca %struct.ScmObj*, align 8
%current_45args54981 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54980)
store volatile %struct.ScmObj* %current_45args54981, %struct.ScmObj** %stackaddr$prim55827, align 8
%stackaddr$prim55828 = alloca %struct.ScmObj*, align 8
%_37_62_6148123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54981)
store volatile %struct.ScmObj* %_37_62_6148123, %struct.ScmObj** %stackaddr$prim55828, align 8
%ae50257 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50258 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55829 = alloca %struct.ScmObj*, align 8
%_37append48119 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50257, %struct.ScmObj* %ae50258)
store volatile %struct.ScmObj* %_37append48119, %struct.ScmObj** %stackaddr$prim55829, align 8
%stackaddr$makeclosure55830 = alloca %struct.ScmObj*, align 8
%fptrToInt55831 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50259 to i64
%ae50259 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55831)
store volatile %struct.ScmObj* %ae50259, %struct.ScmObj** %stackaddr$makeclosure55830, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50259, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50259, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50259, %struct.ScmObj* %_37append48119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50259, %struct.ScmObj* %_37foldl48129, i64 3)
%ae50260 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55832 = alloca %struct.ScmObj*, align 8
%fptrToInt55833 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50261 to i64
%ae50261 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55833)
store volatile %struct.ScmObj* %ae50261, %struct.ScmObj** %stackaddr$makeclosure55832, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50261, %struct.ScmObj* %_37append48119, i64 0)
%argslist55229$ae502590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55834 = alloca %struct.ScmObj*, align 8
%argslist55229$ae502591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50261, %struct.ScmObj* %argslist55229$ae502590)
store volatile %struct.ScmObj* %argslist55229$ae502591, %struct.ScmObj** %stackaddr$prim55834, align 8
%stackaddr$prim55835 = alloca %struct.ScmObj*, align 8
%argslist55229$ae502592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50260, %struct.ScmObj* %argslist55229$ae502591)
store volatile %struct.ScmObj* %argslist55229$ae502592, %struct.ScmObj** %stackaddr$prim55835, align 8
%clofunc55836 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50259)
musttail call tailcc void %clofunc55836(%struct.ScmObj* %ae50259, %struct.ScmObj* %argslist55229$ae502592)
ret void
}

define tailcc void @proc_clo$ae50259(%struct.ScmObj* %env$ae50259,%struct.ScmObj* %current_45args54983) {
%stackaddr$env-ref55837 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50259, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55837
%stackaddr$env-ref55838 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50259, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55838
%stackaddr$env-ref55839 = alloca %struct.ScmObj*, align 8
%_37append48119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50259, i64 2)
store %struct.ScmObj* %_37append48119, %struct.ScmObj** %stackaddr$env-ref55839
%stackaddr$env-ref55840 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50259, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55840
%stackaddr$prim55841 = alloca %struct.ScmObj*, align 8
%_95k48322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54983)
store volatile %struct.ScmObj* %_95k48322, %struct.ScmObj** %stackaddr$prim55841, align 8
%stackaddr$prim55842 = alloca %struct.ScmObj*, align 8
%current_45args54984 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54983)
store volatile %struct.ScmObj* %current_45args54984, %struct.ScmObj** %stackaddr$prim55842, align 8
%stackaddr$prim55843 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54984)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim55843, align 8
%ae50327 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55844 = alloca %struct.ScmObj*, align 8
%_95048120 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48119, %struct.ScmObj* %ae50327, %struct.ScmObj* %anf_45bind48239)
store volatile %struct.ScmObj* %_95048120, %struct.ScmObj** %stackaddr$prim55844, align 8
%ae50330 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55845 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48119, %struct.ScmObj* %ae50330)
store volatile %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$prim55845, align 8
%stackaddr$makeclosure55846 = alloca %struct.ScmObj*, align 8
%fptrToInt55847 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50331 to i64
%ae50331 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55847)
store volatile %struct.ScmObj* %ae50331, %struct.ScmObj** %stackaddr$makeclosure55846, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50331, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50331, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50331, %struct.ScmObj* %_37foldl48129, i64 2)
%ae50332 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55848 = alloca %struct.ScmObj*, align 8
%fptrToInt55849 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50333 to i64
%ae50333 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55849)
store volatile %struct.ScmObj* %ae50333, %struct.ScmObj** %stackaddr$makeclosure55848, align 8
%argslist55218$ae503310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55850 = alloca %struct.ScmObj*, align 8
%argslist55218$ae503311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50333, %struct.ScmObj* %argslist55218$ae503310)
store volatile %struct.ScmObj* %argslist55218$ae503311, %struct.ScmObj** %stackaddr$prim55850, align 8
%stackaddr$prim55851 = alloca %struct.ScmObj*, align 8
%argslist55218$ae503312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50332, %struct.ScmObj* %argslist55218$ae503311)
store volatile %struct.ScmObj* %argslist55218$ae503312, %struct.ScmObj** %stackaddr$prim55851, align 8
%clofunc55852 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50331)
musttail call tailcc void %clofunc55852(%struct.ScmObj* %ae50331, %struct.ScmObj* %argslist55218$ae503312)
ret void
}

define tailcc void @proc_clo$ae50331(%struct.ScmObj* %env$ae50331,%struct.ScmObj* %current_45args54986) {
%stackaddr$env-ref55853 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50331, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55853
%stackaddr$env-ref55854 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50331, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55854
%stackaddr$env-ref55855 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50331, i64 2)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55855
%stackaddr$prim55856 = alloca %struct.ScmObj*, align 8
%_95k48323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54986)
store volatile %struct.ScmObj* %_95k48323, %struct.ScmObj** %stackaddr$prim55856, align 8
%stackaddr$prim55857 = alloca %struct.ScmObj*, align 8
%current_45args54987 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54986)
store volatile %struct.ScmObj* %current_45args54987, %struct.ScmObj** %stackaddr$prim55857, align 8
%stackaddr$prim55858 = alloca %struct.ScmObj*, align 8
%_37list_6348111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54987)
store volatile %struct.ScmObj* %_37list_6348111, %struct.ScmObj** %stackaddr$prim55858, align 8
%stackaddr$makeclosure55859 = alloca %struct.ScmObj*, align 8
%fptrToInt55860 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50747 to i64
%ae50747 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55860)
store volatile %struct.ScmObj* %ae50747, %struct.ScmObj** %stackaddr$makeclosure55859, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50747, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50747, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50747, %struct.ScmObj* %_37foldl48129, i64 2)
%ae50748 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55861 = alloca %struct.ScmObj*, align 8
%fptrToInt55862 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50749 to i64
%ae50749 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55862)
store volatile %struct.ScmObj* %ae50749, %struct.ScmObj** %stackaddr$makeclosure55861, align 8
%argslist55193$ae507470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55863 = alloca %struct.ScmObj*, align 8
%argslist55193$ae507471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50749, %struct.ScmObj* %argslist55193$ae507470)
store volatile %struct.ScmObj* %argslist55193$ae507471, %struct.ScmObj** %stackaddr$prim55863, align 8
%stackaddr$prim55864 = alloca %struct.ScmObj*, align 8
%argslist55193$ae507472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50748, %struct.ScmObj* %argslist55193$ae507471)
store volatile %struct.ScmObj* %argslist55193$ae507472, %struct.ScmObj** %stackaddr$prim55864, align 8
%clofunc55865 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50747)
musttail call tailcc void %clofunc55865(%struct.ScmObj* %ae50747, %struct.ScmObj* %argslist55193$ae507472)
ret void
}

define tailcc void @proc_clo$ae50747(%struct.ScmObj* %env$ae50747,%struct.ScmObj* %current_45args54989) {
%stackaddr$env-ref55866 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50747, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55866
%stackaddr$env-ref55867 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50747, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55867
%stackaddr$env-ref55868 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50747, i64 2)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55868
%stackaddr$prim55869 = alloca %struct.ScmObj*, align 8
%_95k48324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54989)
store volatile %struct.ScmObj* %_95k48324, %struct.ScmObj** %stackaddr$prim55869, align 8
%stackaddr$prim55870 = alloca %struct.ScmObj*, align 8
%current_45args54990 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54989)
store volatile %struct.ScmObj* %current_45args54990, %struct.ScmObj** %stackaddr$prim55870, align 8
%stackaddr$prim55871 = alloca %struct.ScmObj*, align 8
%_37drop48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54990)
store volatile %struct.ScmObj* %_37drop48102, %struct.ScmObj** %stackaddr$prim55871, align 8
%stackaddr$makeclosure55872 = alloca %struct.ScmObj*, align 8
%fptrToInt55873 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51283 to i64
%ae51283 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55873)
store volatile %struct.ScmObj* %ae51283, %struct.ScmObj** %stackaddr$makeclosure55872, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51283, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51283, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51283, %struct.ScmObj* %_37foldl48129, i64 2)
%ae51284 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55874 = alloca %struct.ScmObj*, align 8
%fptrToInt55875 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51285 to i64
%ae51285 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55875)
store volatile %struct.ScmObj* %ae51285, %struct.ScmObj** %stackaddr$makeclosure55874, align 8
%argslist55169$ae512830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55876 = alloca %struct.ScmObj*, align 8
%argslist55169$ae512831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51285, %struct.ScmObj* %argslist55169$ae512830)
store volatile %struct.ScmObj* %argslist55169$ae512831, %struct.ScmObj** %stackaddr$prim55876, align 8
%stackaddr$prim55877 = alloca %struct.ScmObj*, align 8
%argslist55169$ae512832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51284, %struct.ScmObj* %argslist55169$ae512831)
store volatile %struct.ScmObj* %argslist55169$ae512832, %struct.ScmObj** %stackaddr$prim55877, align 8
%clofunc55878 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51283)
musttail call tailcc void %clofunc55878(%struct.ScmObj* %ae51283, %struct.ScmObj* %argslist55169$ae512832)
ret void
}

define tailcc void @proc_clo$ae51283(%struct.ScmObj* %env$ae51283,%struct.ScmObj* %current_45args54992) {
%stackaddr$env-ref55879 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51283, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55879
%stackaddr$env-ref55880 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51283, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55880
%stackaddr$env-ref55881 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51283, i64 2)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55881
%stackaddr$prim55882 = alloca %struct.ScmObj*, align 8
%_95k48325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54992)
store volatile %struct.ScmObj* %_95k48325, %struct.ScmObj** %stackaddr$prim55882, align 8
%stackaddr$prim55883 = alloca %struct.ScmObj*, align 8
%current_45args54993 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54992)
store volatile %struct.ScmObj* %current_45args54993, %struct.ScmObj** %stackaddr$prim55883, align 8
%stackaddr$prim55884 = alloca %struct.ScmObj*, align 8
%_37memv48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54993)
store volatile %struct.ScmObj* %_37memv48095, %struct.ScmObj** %stackaddr$prim55884, align 8
%stackaddr$makeclosure55885 = alloca %struct.ScmObj*, align 8
%fptrToInt55886 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51687 to i64
%ae51687 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55886)
store volatile %struct.ScmObj* %ae51687, %struct.ScmObj** %stackaddr$makeclosure55885, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51687, %struct.ScmObj* %_37foldl48129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51687, %struct.ScmObj* %_37foldr148047, i64 1)
%ae51688 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55887 = alloca %struct.ScmObj*, align 8
%fptrToInt55888 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51689 to i64
%ae51689 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55888)
store volatile %struct.ScmObj* %ae51689, %struct.ScmObj** %stackaddr$makeclosure55887, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51689, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist55143$ae516870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55889 = alloca %struct.ScmObj*, align 8
%argslist55143$ae516871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51689, %struct.ScmObj* %argslist55143$ae516870)
store volatile %struct.ScmObj* %argslist55143$ae516871, %struct.ScmObj** %stackaddr$prim55889, align 8
%stackaddr$prim55890 = alloca %struct.ScmObj*, align 8
%argslist55143$ae516872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51688, %struct.ScmObj* %argslist55143$ae516871)
store volatile %struct.ScmObj* %argslist55143$ae516872, %struct.ScmObj** %stackaddr$prim55890, align 8
%clofunc55891 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51687)
musttail call tailcc void %clofunc55891(%struct.ScmObj* %ae51687, %struct.ScmObj* %argslist55143$ae516872)
ret void
}

define tailcc void @proc_clo$ae51687(%struct.ScmObj* %env$ae51687,%struct.ScmObj* %current_45args54995) {
%stackaddr$env-ref55892 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51687, i64 0)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55892
%stackaddr$env-ref55893 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51687, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55893
%stackaddr$prim55894 = alloca %struct.ScmObj*, align 8
%_95k48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54995)
store volatile %struct.ScmObj* %_95k48326, %struct.ScmObj** %stackaddr$prim55894, align 8
%stackaddr$prim55895 = alloca %struct.ScmObj*, align 8
%current_45args54996 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54995)
store volatile %struct.ScmObj* %current_45args54996, %struct.ScmObj** %stackaddr$prim55895, align 8
%stackaddr$prim55896 = alloca %struct.ScmObj*, align 8
%_37_4748091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54996)
store volatile %struct.ScmObj* %_37_4748091, %struct.ScmObj** %stackaddr$prim55896, align 8
%stackaddr$makeclosure55897 = alloca %struct.ScmObj*, align 8
%fptrToInt55898 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51785 to i64
%ae51785 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55898)
store volatile %struct.ScmObj* %ae51785, %struct.ScmObj** %stackaddr$makeclosure55897, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51785, %struct.ScmObj* %_37foldl48129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51785, %struct.ScmObj* %_37foldr148047, i64 1)
%ae51786 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55899 = alloca %struct.ScmObj*, align 8
%fptrToInt55900 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51787 to i64
%ae51787 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55900)
store volatile %struct.ScmObj* %ae51787, %struct.ScmObj** %stackaddr$makeclosure55899, align 8
%argslist55130$ae517850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55901 = alloca %struct.ScmObj*, align 8
%argslist55130$ae517851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51787, %struct.ScmObj* %argslist55130$ae517850)
store volatile %struct.ScmObj* %argslist55130$ae517851, %struct.ScmObj** %stackaddr$prim55901, align 8
%stackaddr$prim55902 = alloca %struct.ScmObj*, align 8
%argslist55130$ae517852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51786, %struct.ScmObj* %argslist55130$ae517851)
store volatile %struct.ScmObj* %argslist55130$ae517852, %struct.ScmObj** %stackaddr$prim55902, align 8
%clofunc55903 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51785)
musttail call tailcc void %clofunc55903(%struct.ScmObj* %ae51785, %struct.ScmObj* %argslist55130$ae517852)
ret void
}

define tailcc void @proc_clo$ae51785(%struct.ScmObj* %env$ae51785,%struct.ScmObj* %current_45args54998) {
%stackaddr$env-ref55904 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51785, i64 0)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55904
%stackaddr$env-ref55905 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51785, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55905
%stackaddr$prim55906 = alloca %struct.ScmObj*, align 8
%_95k48327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54998)
store volatile %struct.ScmObj* %_95k48327, %struct.ScmObj** %stackaddr$prim55906, align 8
%stackaddr$prim55907 = alloca %struct.ScmObj*, align 8
%current_45args54999 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54998)
store volatile %struct.ScmObj* %current_45args54999, %struct.ScmObj** %stackaddr$prim55907, align 8
%stackaddr$prim55908 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54999)
store volatile %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$prim55908, align 8
%stackaddr$makeclosure55909 = alloca %struct.ScmObj*, align 8
%fptrToInt55910 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51805 to i64
%ae51805 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55910)
store volatile %struct.ScmObj* %ae51805, %struct.ScmObj** %stackaddr$makeclosure55909, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51805, %struct.ScmObj* %_37first48089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51805, %struct.ScmObj* %_37foldl48129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51805, %struct.ScmObj* %_37foldr148047, i64 2)
%ae51806 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55911 = alloca %struct.ScmObj*, align 8
%fptrToInt55912 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51807 to i64
%ae51807 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55912)
store volatile %struct.ScmObj* %ae51807, %struct.ScmObj** %stackaddr$makeclosure55911, align 8
%argslist55125$ae518050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55913 = alloca %struct.ScmObj*, align 8
%argslist55125$ae518051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51807, %struct.ScmObj* %argslist55125$ae518050)
store volatile %struct.ScmObj* %argslist55125$ae518051, %struct.ScmObj** %stackaddr$prim55913, align 8
%stackaddr$prim55914 = alloca %struct.ScmObj*, align 8
%argslist55125$ae518052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51806, %struct.ScmObj* %argslist55125$ae518051)
store volatile %struct.ScmObj* %argslist55125$ae518052, %struct.ScmObj** %stackaddr$prim55914, align 8
%clofunc55915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51805)
musttail call tailcc void %clofunc55915(%struct.ScmObj* %ae51805, %struct.ScmObj* %argslist55125$ae518052)
ret void
}

define tailcc void @proc_clo$ae51805(%struct.ScmObj* %env$ae51805,%struct.ScmObj* %current_45args55001) {
%stackaddr$env-ref55916 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51805, i64 0)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref55916
%stackaddr$env-ref55917 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51805, i64 1)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55917
%stackaddr$env-ref55918 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51805, i64 2)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55918
%stackaddr$prim55919 = alloca %struct.ScmObj*, align 8
%_95k48328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55001)
store volatile %struct.ScmObj* %_95k48328, %struct.ScmObj** %stackaddr$prim55919, align 8
%stackaddr$prim55920 = alloca %struct.ScmObj*, align 8
%current_45args55002 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55001)
store volatile %struct.ScmObj* %current_45args55002, %struct.ScmObj** %stackaddr$prim55920, align 8
%stackaddr$prim55921 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55002)
store volatile %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$prim55921, align 8
%stackaddr$makeclosure55922 = alloca %struct.ScmObj*, align 8
%fptrToInt55923 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51827 to i64
%ae51827 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55923)
store volatile %struct.ScmObj* %ae51827, %struct.ScmObj** %stackaddr$makeclosure55922, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51827, %struct.ScmObj* %_37first48089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51827, %struct.ScmObj* %_37second48087, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51827, %struct.ScmObj* %_37foldl48129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51827, %struct.ScmObj* %_37foldr148047, i64 3)
%ae51828 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55924 = alloca %struct.ScmObj*, align 8
%fptrToInt55925 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51829 to i64
%ae51829 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55925)
store volatile %struct.ScmObj* %ae51829, %struct.ScmObj** %stackaddr$makeclosure55924, align 8
%argslist55120$ae518270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55926 = alloca %struct.ScmObj*, align 8
%argslist55120$ae518271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51829, %struct.ScmObj* %argslist55120$ae518270)
store volatile %struct.ScmObj* %argslist55120$ae518271, %struct.ScmObj** %stackaddr$prim55926, align 8
%stackaddr$prim55927 = alloca %struct.ScmObj*, align 8
%argslist55120$ae518272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51828, %struct.ScmObj* %argslist55120$ae518271)
store volatile %struct.ScmObj* %argslist55120$ae518272, %struct.ScmObj** %stackaddr$prim55927, align 8
%clofunc55928 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51827)
musttail call tailcc void %clofunc55928(%struct.ScmObj* %ae51827, %struct.ScmObj* %argslist55120$ae518272)
ret void
}

define tailcc void @proc_clo$ae51827(%struct.ScmObj* %env$ae51827,%struct.ScmObj* %current_45args55004) {
%stackaddr$env-ref55929 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51827, i64 0)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref55929
%stackaddr$env-ref55930 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51827, i64 1)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref55930
%stackaddr$env-ref55931 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51827, i64 2)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55931
%stackaddr$env-ref55932 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51827, i64 3)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55932
%stackaddr$prim55933 = alloca %struct.ScmObj*, align 8
%_95k48329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55004)
store volatile %struct.ScmObj* %_95k48329, %struct.ScmObj** %stackaddr$prim55933, align 8
%stackaddr$prim55934 = alloca %struct.ScmObj*, align 8
%current_45args55005 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55004)
store volatile %struct.ScmObj* %current_45args55005, %struct.ScmObj** %stackaddr$prim55934, align 8
%stackaddr$prim55935 = alloca %struct.ScmObj*, align 8
%_37third48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55005)
store volatile %struct.ScmObj* %_37third48085, %struct.ScmObj** %stackaddr$prim55935, align 8
%stackaddr$makeclosure55936 = alloca %struct.ScmObj*, align 8
%fptrToInt55937 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51851 to i64
%ae51851 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55937)
store volatile %struct.ScmObj* %ae51851, %struct.ScmObj** %stackaddr$makeclosure55936, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51851, %struct.ScmObj* %_37first48089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51851, %struct.ScmObj* %_37second48087, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51851, %struct.ScmObj* %_37foldl48129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51851, %struct.ScmObj* %_37foldr148047, i64 3)
%ae51852 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55938 = alloca %struct.ScmObj*, align 8
%fptrToInt55939 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51853 to i64
%ae51853 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55939)
store volatile %struct.ScmObj* %ae51853, %struct.ScmObj** %stackaddr$makeclosure55938, align 8
%argslist55115$ae518510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55940 = alloca %struct.ScmObj*, align 8
%argslist55115$ae518511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51853, %struct.ScmObj* %argslist55115$ae518510)
store volatile %struct.ScmObj* %argslist55115$ae518511, %struct.ScmObj** %stackaddr$prim55940, align 8
%stackaddr$prim55941 = alloca %struct.ScmObj*, align 8
%argslist55115$ae518512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51852, %struct.ScmObj* %argslist55115$ae518511)
store volatile %struct.ScmObj* %argslist55115$ae518512, %struct.ScmObj** %stackaddr$prim55941, align 8
%clofunc55942 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51851)
musttail call tailcc void %clofunc55942(%struct.ScmObj* %ae51851, %struct.ScmObj* %argslist55115$ae518512)
ret void
}

define tailcc void @proc_clo$ae51851(%struct.ScmObj* %env$ae51851,%struct.ScmObj* %current_45args55007) {
%stackaddr$env-ref55943 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51851, i64 0)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref55943
%stackaddr$env-ref55944 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51851, i64 1)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref55944
%stackaddr$env-ref55945 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51851, i64 2)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55945
%stackaddr$env-ref55946 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51851, i64 3)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55946
%stackaddr$prim55947 = alloca %struct.ScmObj*, align 8
%_95k48330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55007)
store volatile %struct.ScmObj* %_95k48330, %struct.ScmObj** %stackaddr$prim55947, align 8
%stackaddr$prim55948 = alloca %struct.ScmObj*, align 8
%current_45args55008 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55007)
store volatile %struct.ScmObj* %current_45args55008, %struct.ScmObj** %stackaddr$prim55948, align 8
%stackaddr$prim55949 = alloca %struct.ScmObj*, align 8
%_37fourth48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55008)
store volatile %struct.ScmObj* %_37fourth48083, %struct.ScmObj** %stackaddr$prim55949, align 8
%stackaddr$makeclosure55950 = alloca %struct.ScmObj*, align 8
%fptrToInt55951 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51877 to i64
%ae51877 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55951)
store volatile %struct.ScmObj* %ae51877, %struct.ScmObj** %stackaddr$makeclosure55950, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51877, %struct.ScmObj* %_37first48089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51877, %struct.ScmObj* %_37second48087, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51877, %struct.ScmObj* %_37foldl48129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51877, %struct.ScmObj* %_37foldr148047, i64 3)
%ae51878 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55952 = alloca %struct.ScmObj*, align 8
%fptrToInt55953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51879 to i64
%ae51879 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55953)
store volatile %struct.ScmObj* %ae51879, %struct.ScmObj** %stackaddr$makeclosure55952, align 8
%argslist55110$ae518770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55954 = alloca %struct.ScmObj*, align 8
%argslist55110$ae518771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51879, %struct.ScmObj* %argslist55110$ae518770)
store volatile %struct.ScmObj* %argslist55110$ae518771, %struct.ScmObj** %stackaddr$prim55954, align 8
%stackaddr$prim55955 = alloca %struct.ScmObj*, align 8
%argslist55110$ae518772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51878, %struct.ScmObj* %argslist55110$ae518771)
store volatile %struct.ScmObj* %argslist55110$ae518772, %struct.ScmObj** %stackaddr$prim55955, align 8
%clofunc55956 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51877)
musttail call tailcc void %clofunc55956(%struct.ScmObj* %ae51877, %struct.ScmObj* %argslist55110$ae518772)
ret void
}

define tailcc void @proc_clo$ae51877(%struct.ScmObj* %env$ae51877,%struct.ScmObj* %current_45args55010) {
%stackaddr$env-ref55957 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51877, i64 0)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref55957
%stackaddr$env-ref55958 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51877, i64 1)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref55958
%stackaddr$env-ref55959 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51877, i64 2)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55959
%stackaddr$env-ref55960 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51877, i64 3)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55960
%stackaddr$prim55961 = alloca %struct.ScmObj*, align 8
%_95k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55010)
store volatile %struct.ScmObj* %_95k48331, %struct.ScmObj** %stackaddr$prim55961, align 8
%stackaddr$prim55962 = alloca %struct.ScmObj*, align 8
%current_45args55011 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55010)
store volatile %struct.ScmObj* %current_45args55011, %struct.ScmObj** %stackaddr$prim55962, align 8
%stackaddr$prim55963 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55011)
store volatile %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$prim55963, align 8
%stackaddr$makeclosure55964 = alloca %struct.ScmObj*, align 8
%fptrToInt55965 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51915 to i64
%ae51915 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55965)
store volatile %struct.ScmObj* %ae51915, %struct.ScmObj** %stackaddr$makeclosure55964, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51915, %struct.ScmObj* %_37first48089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51915, %struct.ScmObj* %_37second48087, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51915, %struct.ScmObj* %anf_45bind48278, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51915, %struct.ScmObj* %_37foldl48129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51915, %struct.ScmObj* %_37foldr148047, i64 4)
%ae51916 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55966 = alloca %struct.ScmObj*, align 8
%fptrToInt55967 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51917 to i64
%ae51917 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55967)
store volatile %struct.ScmObj* %ae51917, %struct.ScmObj** %stackaddr$makeclosure55966, align 8
%argslist55100$ae519150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55968 = alloca %struct.ScmObj*, align 8
%argslist55100$ae519151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51917, %struct.ScmObj* %argslist55100$ae519150)
store volatile %struct.ScmObj* %argslist55100$ae519151, %struct.ScmObj** %stackaddr$prim55968, align 8
%stackaddr$prim55969 = alloca %struct.ScmObj*, align 8
%argslist55100$ae519152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51916, %struct.ScmObj* %argslist55100$ae519151)
store volatile %struct.ScmObj* %argslist55100$ae519152, %struct.ScmObj** %stackaddr$prim55969, align 8
%clofunc55970 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51915)
musttail call tailcc void %clofunc55970(%struct.ScmObj* %ae51915, %struct.ScmObj* %argslist55100$ae519152)
ret void
}

define tailcc void @proc_clo$ae51915(%struct.ScmObj* %env$ae51915,%struct.ScmObj* %current_45args55013) {
%stackaddr$env-ref55971 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51915, i64 0)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref55971
%stackaddr$env-ref55972 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51915, i64 1)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref55972
%stackaddr$env-ref55973 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51915, i64 2)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref55973
%stackaddr$env-ref55974 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51915, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55974
%stackaddr$env-ref55975 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51915, i64 4)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55975
%stackaddr$prim55976 = alloca %struct.ScmObj*, align 8
%_95k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55013)
store volatile %struct.ScmObj* %_95k48332, %struct.ScmObj** %stackaddr$prim55976, align 8
%stackaddr$prim55977 = alloca %struct.ScmObj*, align 8
%current_45args55014 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55013)
store volatile %struct.ScmObj* %current_45args55014, %struct.ScmObj** %stackaddr$prim55977, align 8
%stackaddr$prim55978 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55014)
store volatile %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$prim55978, align 8
%stackaddr$makeclosure55979 = alloca %struct.ScmObj*, align 8
%fptrToInt55980 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51937 to i64
%ae51937 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55980)
store volatile %struct.ScmObj* %ae51937, %struct.ScmObj** %stackaddr$makeclosure55979, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51937, %struct.ScmObj* %_37second48087, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51937, %struct.ScmObj* %anf_45bind48279, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51937, %struct.ScmObj* %_37first48089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51937, %struct.ScmObj* %anf_45bind48278, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51937, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51937, %struct.ScmObj* %_37foldr148047, i64 5)
%ae51938 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55981 = alloca %struct.ScmObj*, align 8
%fptrToInt55982 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51939 to i64
%ae51939 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55982)
store volatile %struct.ScmObj* %ae51939, %struct.ScmObj** %stackaddr$makeclosure55981, align 8
%argslist55098$ae519370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55983 = alloca %struct.ScmObj*, align 8
%argslist55098$ae519371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51939, %struct.ScmObj* %argslist55098$ae519370)
store volatile %struct.ScmObj* %argslist55098$ae519371, %struct.ScmObj** %stackaddr$prim55983, align 8
%stackaddr$prim55984 = alloca %struct.ScmObj*, align 8
%argslist55098$ae519372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51938, %struct.ScmObj* %argslist55098$ae519371)
store volatile %struct.ScmObj* %argslist55098$ae519372, %struct.ScmObj** %stackaddr$prim55984, align 8
%clofunc55985 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51937)
musttail call tailcc void %clofunc55985(%struct.ScmObj* %ae51937, %struct.ScmObj* %argslist55098$ae519372)
ret void
}

define tailcc void @proc_clo$ae51937(%struct.ScmObj* %env$ae51937,%struct.ScmObj* %current_45args55016) {
%stackaddr$env-ref55986 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51937, i64 0)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref55986
%stackaddr$env-ref55987 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51937, i64 1)
store %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$env-ref55987
%stackaddr$env-ref55988 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51937, i64 2)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref55988
%stackaddr$env-ref55989 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51937, i64 3)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref55989
%stackaddr$env-ref55990 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51937, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55990
%stackaddr$env-ref55991 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51937, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55991
%stackaddr$prim55992 = alloca %struct.ScmObj*, align 8
%_95k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55016)
store volatile %struct.ScmObj* %_95k48333, %struct.ScmObj** %stackaddr$prim55992, align 8
%stackaddr$prim55993 = alloca %struct.ScmObj*, align 8
%current_45args55017 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55016)
store volatile %struct.ScmObj* %current_45args55017, %struct.ScmObj** %stackaddr$prim55993, align 8
%stackaddr$prim55994 = alloca %struct.ScmObj*, align 8
%anf_45bind48280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55017)
store volatile %struct.ScmObj* %anf_45bind48280, %struct.ScmObj** %stackaddr$prim55994, align 8
%stackaddr$makeclosure55995 = alloca %struct.ScmObj*, align 8
%fptrToInt55996 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51960 to i64
%ae51960 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55996)
store volatile %struct.ScmObj* %ae51960, %struct.ScmObj** %stackaddr$makeclosure55995, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %_37second48087, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %anf_45bind48279, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %_37first48089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %anf_45bind48278, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %_37foldr148047, i64 5)
%ae51961 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51962 = call %struct.ScmObj* @const_init_int(i64 1)
%ae51963 = call %struct.ScmObj* @const_init_int(i64 2)
%ae51964 = call %struct.ScmObj* @const_init_int(i64 3)
%argslist55096$anf_45bind482800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55997 = alloca %struct.ScmObj*, align 8
%argslist55096$anf_45bind482801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51964, %struct.ScmObj* %argslist55096$anf_45bind482800)
store volatile %struct.ScmObj* %argslist55096$anf_45bind482801, %struct.ScmObj** %stackaddr$prim55997, align 8
%stackaddr$prim55998 = alloca %struct.ScmObj*, align 8
%argslist55096$anf_45bind482802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51963, %struct.ScmObj* %argslist55096$anf_45bind482801)
store volatile %struct.ScmObj* %argslist55096$anf_45bind482802, %struct.ScmObj** %stackaddr$prim55998, align 8
%stackaddr$prim55999 = alloca %struct.ScmObj*, align 8
%argslist55096$anf_45bind482803 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51962, %struct.ScmObj* %argslist55096$anf_45bind482802)
store volatile %struct.ScmObj* %argslist55096$anf_45bind482803, %struct.ScmObj** %stackaddr$prim55999, align 8
%stackaddr$prim56000 = alloca %struct.ScmObj*, align 8
%argslist55096$anf_45bind482804 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51961, %struct.ScmObj* %argslist55096$anf_45bind482803)
store volatile %struct.ScmObj* %argslist55096$anf_45bind482804, %struct.ScmObj** %stackaddr$prim56000, align 8
%stackaddr$prim56001 = alloca %struct.ScmObj*, align 8
%argslist55096$anf_45bind482805 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51960, %struct.ScmObj* %argslist55096$anf_45bind482804)
store volatile %struct.ScmObj* %argslist55096$anf_45bind482805, %struct.ScmObj** %stackaddr$prim56001, align 8
%clofunc56002 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48280)
musttail call tailcc void %clofunc56002(%struct.ScmObj* %anf_45bind48280, %struct.ScmObj* %argslist55096$anf_45bind482805)
ret void
}

define tailcc void @proc_clo$ae51960(%struct.ScmObj* %env$ae51960,%struct.ScmObj* %current_45args55019) {
%stackaddr$env-ref56003 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 0)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref56003
%stackaddr$env-ref56004 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 1)
store %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$env-ref56004
%stackaddr$env-ref56005 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 2)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref56005
%stackaddr$env-ref56006 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 3)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref56006
%stackaddr$env-ref56007 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56007
%stackaddr$env-ref56008 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56008
%stackaddr$prim56009 = alloca %struct.ScmObj*, align 8
%_95k48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55019)
store volatile %struct.ScmObj* %_95k48334, %struct.ScmObj** %stackaddr$prim56009, align 8
%stackaddr$prim56010 = alloca %struct.ScmObj*, align 8
%current_45args55020 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55019)
store volatile %struct.ScmObj* %current_45args55020, %struct.ScmObj** %stackaddr$prim56010, align 8
%stackaddr$prim56011 = alloca %struct.ScmObj*, align 8
%anf_45bind48281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55020)
store volatile %struct.ScmObj* %anf_45bind48281, %struct.ScmObj** %stackaddr$prim56011, align 8
%stackaddr$makeclosure56012 = alloca %struct.ScmObj*, align 8
%fptrToInt56013 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51989 to i64
%ae51989 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56013)
store volatile %struct.ScmObj* %ae51989, %struct.ScmObj** %stackaddr$makeclosure56012, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %_37first48089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %anf_45bind48281, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %_37second48087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %anf_45bind48279, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %anf_45bind48278, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %_37foldr148047, i64 6)
%ae51990 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56014 = alloca %struct.ScmObj*, align 8
%fptrToInt56015 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51991 to i64
%ae51991 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56015)
store volatile %struct.ScmObj* %ae51991, %struct.ScmObj** %stackaddr$makeclosure56014, align 8
%argslist55095$ae519890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56016 = alloca %struct.ScmObj*, align 8
%argslist55095$ae519891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51991, %struct.ScmObj* %argslist55095$ae519890)
store volatile %struct.ScmObj* %argslist55095$ae519891, %struct.ScmObj** %stackaddr$prim56016, align 8
%stackaddr$prim56017 = alloca %struct.ScmObj*, align 8
%argslist55095$ae519892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51990, %struct.ScmObj* %argslist55095$ae519891)
store volatile %struct.ScmObj* %argslist55095$ae519892, %struct.ScmObj** %stackaddr$prim56017, align 8
%clofunc56018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51989)
musttail call tailcc void %clofunc56018(%struct.ScmObj* %ae51989, %struct.ScmObj* %argslist55095$ae519892)
ret void
}

define tailcc void @proc_clo$ae51989(%struct.ScmObj* %env$ae51989,%struct.ScmObj* %current_45args55022) {
%stackaddr$env-ref56019 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 0)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref56019
%stackaddr$env-ref56020 = alloca %struct.ScmObj*, align 8
%anf_45bind48281 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 1)
store %struct.ScmObj* %anf_45bind48281, %struct.ScmObj** %stackaddr$env-ref56020
%stackaddr$env-ref56021 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 2)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref56021
%stackaddr$env-ref56022 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 3)
store %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$env-ref56022
%stackaddr$env-ref56023 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 4)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref56023
%stackaddr$env-ref56024 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56024
%stackaddr$env-ref56025 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 6)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56025
%stackaddr$prim56026 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55022)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim56026, align 8
%stackaddr$prim56027 = alloca %struct.ScmObj*, align 8
%current_45args55023 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55022)
store volatile %struct.ScmObj* %current_45args55023, %struct.ScmObj** %stackaddr$prim56027, align 8
%stackaddr$prim56028 = alloca %struct.ScmObj*, align 8
%anf_45bind48282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55023)
store volatile %struct.ScmObj* %anf_45bind48282, %struct.ScmObj** %stackaddr$prim56028, align 8
%stackaddr$makeclosure56029 = alloca %struct.ScmObj*, align 8
%fptrToInt56030 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52012 to i64
%ae52012 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56030)
store volatile %struct.ScmObj* %ae52012, %struct.ScmObj** %stackaddr$makeclosure56029, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %_37first48089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %anf_45bind48281, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %_37second48087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %anf_45bind48279, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %anf_45bind48278, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %_37foldr148047, i64 6)
%ae52013 = call %struct.ScmObj* @const_init_int(i64 4)
%ae52014 = call %struct.ScmObj* @const_init_int(i64 5)
%ae52015 = call %struct.ScmObj* @const_init_int(i64 6)
%ae52016 = call %struct.ScmObj* @const_init_int(i64 7)
%argslist55093$anf_45bind482820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56031 = alloca %struct.ScmObj*, align 8
%argslist55093$anf_45bind482821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52016, %struct.ScmObj* %argslist55093$anf_45bind482820)
store volatile %struct.ScmObj* %argslist55093$anf_45bind482821, %struct.ScmObj** %stackaddr$prim56031, align 8
%stackaddr$prim56032 = alloca %struct.ScmObj*, align 8
%argslist55093$anf_45bind482822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52015, %struct.ScmObj* %argslist55093$anf_45bind482821)
store volatile %struct.ScmObj* %argslist55093$anf_45bind482822, %struct.ScmObj** %stackaddr$prim56032, align 8
%stackaddr$prim56033 = alloca %struct.ScmObj*, align 8
%argslist55093$anf_45bind482823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52014, %struct.ScmObj* %argslist55093$anf_45bind482822)
store volatile %struct.ScmObj* %argslist55093$anf_45bind482823, %struct.ScmObj** %stackaddr$prim56033, align 8
%stackaddr$prim56034 = alloca %struct.ScmObj*, align 8
%argslist55093$anf_45bind482824 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52013, %struct.ScmObj* %argslist55093$anf_45bind482823)
store volatile %struct.ScmObj* %argslist55093$anf_45bind482824, %struct.ScmObj** %stackaddr$prim56034, align 8
%stackaddr$prim56035 = alloca %struct.ScmObj*, align 8
%argslist55093$anf_45bind482825 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52012, %struct.ScmObj* %argslist55093$anf_45bind482824)
store volatile %struct.ScmObj* %argslist55093$anf_45bind482825, %struct.ScmObj** %stackaddr$prim56035, align 8
%clofunc56036 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48282)
musttail call tailcc void %clofunc56036(%struct.ScmObj* %anf_45bind48282, %struct.ScmObj* %argslist55093$anf_45bind482825)
ret void
}

define tailcc void @proc_clo$ae52012(%struct.ScmObj* %env$ae52012,%struct.ScmObj* %current_45args55025) {
%stackaddr$env-ref56037 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 0)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref56037
%stackaddr$env-ref56038 = alloca %struct.ScmObj*, align 8
%anf_45bind48281 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 1)
store %struct.ScmObj* %anf_45bind48281, %struct.ScmObj** %stackaddr$env-ref56038
%stackaddr$env-ref56039 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 2)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref56039
%stackaddr$env-ref56040 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 3)
store %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$env-ref56040
%stackaddr$env-ref56041 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 4)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref56041
%stackaddr$env-ref56042 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56042
%stackaddr$env-ref56043 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 6)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56043
%stackaddr$prim56044 = alloca %struct.ScmObj*, align 8
%_95k48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55025)
store volatile %struct.ScmObj* %_95k48336, %struct.ScmObj** %stackaddr$prim56044, align 8
%stackaddr$prim56045 = alloca %struct.ScmObj*, align 8
%current_45args55026 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55025)
store volatile %struct.ScmObj* %current_45args55026, %struct.ScmObj** %stackaddr$prim56045, align 8
%stackaddr$prim56046 = alloca %struct.ScmObj*, align 8
%anf_45bind48283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55026)
store volatile %struct.ScmObj* %anf_45bind48283, %struct.ScmObj** %stackaddr$prim56046, align 8
%stackaddr$makeclosure56047 = alloca %struct.ScmObj*, align 8
%fptrToInt56048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52041 to i64
%ae52041 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56048)
store volatile %struct.ScmObj* %ae52041, %struct.ScmObj** %stackaddr$makeclosure56047, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52041, %struct.ScmObj* %_37first48089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52041, %struct.ScmObj* %anf_45bind48281, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52041, %struct.ScmObj* %_37second48087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52041, %struct.ScmObj* %anf_45bind48279, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52041, %struct.ScmObj* %anf_45bind48278, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52041, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52041, %struct.ScmObj* %_37foldr148047, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52041, %struct.ScmObj* %anf_45bind48283, i64 7)
%ae52042 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56049 = alloca %struct.ScmObj*, align 8
%fptrToInt56050 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52043 to i64
%ae52043 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56050)
store volatile %struct.ScmObj* %ae52043, %struct.ScmObj** %stackaddr$makeclosure56049, align 8
%argslist55092$ae520410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56051 = alloca %struct.ScmObj*, align 8
%argslist55092$ae520411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52043, %struct.ScmObj* %argslist55092$ae520410)
store volatile %struct.ScmObj* %argslist55092$ae520411, %struct.ScmObj** %stackaddr$prim56051, align 8
%stackaddr$prim56052 = alloca %struct.ScmObj*, align 8
%argslist55092$ae520412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52042, %struct.ScmObj* %argslist55092$ae520411)
store volatile %struct.ScmObj* %argslist55092$ae520412, %struct.ScmObj** %stackaddr$prim56052, align 8
%clofunc56053 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52041)
musttail call tailcc void %clofunc56053(%struct.ScmObj* %ae52041, %struct.ScmObj* %argslist55092$ae520412)
ret void
}

define tailcc void @proc_clo$ae52041(%struct.ScmObj* %env$ae52041,%struct.ScmObj* %current_45args55028) {
%stackaddr$env-ref56054 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52041, i64 0)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref56054
%stackaddr$env-ref56055 = alloca %struct.ScmObj*, align 8
%anf_45bind48281 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52041, i64 1)
store %struct.ScmObj* %anf_45bind48281, %struct.ScmObj** %stackaddr$env-ref56055
%stackaddr$env-ref56056 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52041, i64 2)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref56056
%stackaddr$env-ref56057 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52041, i64 3)
store %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$env-ref56057
%stackaddr$env-ref56058 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52041, i64 4)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref56058
%stackaddr$env-ref56059 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52041, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56059
%stackaddr$env-ref56060 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52041, i64 6)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56060
%stackaddr$env-ref56061 = alloca %struct.ScmObj*, align 8
%anf_45bind48283 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52041, i64 7)
store %struct.ScmObj* %anf_45bind48283, %struct.ScmObj** %stackaddr$env-ref56061
%stackaddr$prim56062 = alloca %struct.ScmObj*, align 8
%_95k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55028)
store volatile %struct.ScmObj* %_95k48337, %struct.ScmObj** %stackaddr$prim56062, align 8
%stackaddr$prim56063 = alloca %struct.ScmObj*, align 8
%current_45args55029 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55028)
store volatile %struct.ScmObj* %current_45args55029, %struct.ScmObj** %stackaddr$prim56063, align 8
%stackaddr$prim56064 = alloca %struct.ScmObj*, align 8
%anf_45bind48284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55029)
store volatile %struct.ScmObj* %anf_45bind48284, %struct.ScmObj** %stackaddr$prim56064, align 8
%stackaddr$makeclosure56065 = alloca %struct.ScmObj*, align 8
%fptrToInt56066 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52064 to i64
%ae52064 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56066)
store volatile %struct.ScmObj* %ae52064, %struct.ScmObj** %stackaddr$makeclosure56065, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %_37first48089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %anf_45bind48281, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %_37second48087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %anf_45bind48279, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %anf_45bind48278, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %_37foldr148047, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %anf_45bind48283, i64 7)
%ae52065 = call %struct.ScmObj* @const_init_int(i64 2)
%ae52066 = call %struct.ScmObj* @const_init_int(i64 5)
%ae52067 = call %struct.ScmObj* @const_init_int(i64 7)
%ae52068 = call %struct.ScmObj* @const_init_int(i64 9)
%argslist55090$anf_45bind482840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56067 = alloca %struct.ScmObj*, align 8
%argslist55090$anf_45bind482841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52068, %struct.ScmObj* %argslist55090$anf_45bind482840)
store volatile %struct.ScmObj* %argslist55090$anf_45bind482841, %struct.ScmObj** %stackaddr$prim56067, align 8
%stackaddr$prim56068 = alloca %struct.ScmObj*, align 8
%argslist55090$anf_45bind482842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52067, %struct.ScmObj* %argslist55090$anf_45bind482841)
store volatile %struct.ScmObj* %argslist55090$anf_45bind482842, %struct.ScmObj** %stackaddr$prim56068, align 8
%stackaddr$prim56069 = alloca %struct.ScmObj*, align 8
%argslist55090$anf_45bind482843 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52066, %struct.ScmObj* %argslist55090$anf_45bind482842)
store volatile %struct.ScmObj* %argslist55090$anf_45bind482843, %struct.ScmObj** %stackaddr$prim56069, align 8
%stackaddr$prim56070 = alloca %struct.ScmObj*, align 8
%argslist55090$anf_45bind482844 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52065, %struct.ScmObj* %argslist55090$anf_45bind482843)
store volatile %struct.ScmObj* %argslist55090$anf_45bind482844, %struct.ScmObj** %stackaddr$prim56070, align 8
%stackaddr$prim56071 = alloca %struct.ScmObj*, align 8
%argslist55090$anf_45bind482845 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52064, %struct.ScmObj* %argslist55090$anf_45bind482844)
store volatile %struct.ScmObj* %argslist55090$anf_45bind482845, %struct.ScmObj** %stackaddr$prim56071, align 8
%clofunc56072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48284)
musttail call tailcc void %clofunc56072(%struct.ScmObj* %anf_45bind48284, %struct.ScmObj* %argslist55090$anf_45bind482845)
ret void
}

define tailcc void @proc_clo$ae52064(%struct.ScmObj* %env$ae52064,%struct.ScmObj* %current_45args55031) {
%stackaddr$env-ref56073 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 0)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref56073
%stackaddr$env-ref56074 = alloca %struct.ScmObj*, align 8
%anf_45bind48281 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 1)
store %struct.ScmObj* %anf_45bind48281, %struct.ScmObj** %stackaddr$env-ref56074
%stackaddr$env-ref56075 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 2)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref56075
%stackaddr$env-ref56076 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 3)
store %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$env-ref56076
%stackaddr$env-ref56077 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 4)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref56077
%stackaddr$env-ref56078 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56078
%stackaddr$env-ref56079 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 6)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56079
%stackaddr$env-ref56080 = alloca %struct.ScmObj*, align 8
%anf_45bind48283 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 7)
store %struct.ScmObj* %anf_45bind48283, %struct.ScmObj** %stackaddr$env-ref56080
%stackaddr$prim56081 = alloca %struct.ScmObj*, align 8
%_95k48338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55031)
store volatile %struct.ScmObj* %_95k48338, %struct.ScmObj** %stackaddr$prim56081, align 8
%stackaddr$prim56082 = alloca %struct.ScmObj*, align 8
%current_45args55032 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55031)
store volatile %struct.ScmObj* %current_45args55032, %struct.ScmObj** %stackaddr$prim56082, align 8
%stackaddr$prim56083 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55032)
store volatile %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$prim56083, align 8
%stackaddr$makeclosure56084 = alloca %struct.ScmObj*, align 8
%fptrToInt56085 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52094 to i64
%ae52094 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56085)
store volatile %struct.ScmObj* %ae52094, %struct.ScmObj** %stackaddr$makeclosure56084, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52094, %struct.ScmObj* %_37first48089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52094, %struct.ScmObj* %_37second48087, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52094, %struct.ScmObj* %anf_45bind48278, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52094, %struct.ScmObj* %_37foldl48129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52094, %struct.ScmObj* %_37foldr148047, i64 4)
%argslist55089$anf_45bind482790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56086 = alloca %struct.ScmObj*, align 8
%argslist55089$anf_45bind482791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48285, %struct.ScmObj* %argslist55089$anf_45bind482790)
store volatile %struct.ScmObj* %argslist55089$anf_45bind482791, %struct.ScmObj** %stackaddr$prim56086, align 8
%stackaddr$prim56087 = alloca %struct.ScmObj*, align 8
%argslist55089$anf_45bind482792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48283, %struct.ScmObj* %argslist55089$anf_45bind482791)
store volatile %struct.ScmObj* %argslist55089$anf_45bind482792, %struct.ScmObj** %stackaddr$prim56087, align 8
%stackaddr$prim56088 = alloca %struct.ScmObj*, align 8
%argslist55089$anf_45bind482793 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48281, %struct.ScmObj* %argslist55089$anf_45bind482792)
store volatile %struct.ScmObj* %argslist55089$anf_45bind482793, %struct.ScmObj** %stackaddr$prim56088, align 8
%stackaddr$prim56089 = alloca %struct.ScmObj*, align 8
%argslist55089$anf_45bind482794 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52094, %struct.ScmObj* %argslist55089$anf_45bind482793)
store volatile %struct.ScmObj* %argslist55089$anf_45bind482794, %struct.ScmObj** %stackaddr$prim56089, align 8
%clofunc56090 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48279)
musttail call tailcc void %clofunc56090(%struct.ScmObj* %anf_45bind48279, %struct.ScmObj* %argslist55089$anf_45bind482794)
ret void
}

define tailcc void @proc_clo$ae52094(%struct.ScmObj* %env$ae52094,%struct.ScmObj* %current_45args55034) {
%stackaddr$env-ref56091 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52094, i64 0)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref56091
%stackaddr$env-ref56092 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52094, i64 1)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref56092
%stackaddr$env-ref56093 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52094, i64 2)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref56093
%stackaddr$env-ref56094 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52094, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56094
%stackaddr$env-ref56095 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52094, i64 4)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56095
%stackaddr$prim56096 = alloca %struct.ScmObj*, align 8
%_95k48339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55034)
store volatile %struct.ScmObj* %_95k48339, %struct.ScmObj** %stackaddr$prim56096, align 8
%stackaddr$prim56097 = alloca %struct.ScmObj*, align 8
%current_45args55035 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55034)
store volatile %struct.ScmObj* %current_45args55035, %struct.ScmObj** %stackaddr$prim56097, align 8
%stackaddr$prim56098 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55035)
store volatile %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$prim56098, align 8
%stackaddr$makeclosure56099 = alloca %struct.ScmObj*, align 8
%fptrToInt56100 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52098 to i64
%ae52098 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56100)
store volatile %struct.ScmObj* %ae52098, %struct.ScmObj** %stackaddr$makeclosure56099, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52098, %struct.ScmObj* %_37first48089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52098, %struct.ScmObj* %_37second48087, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52098, %struct.ScmObj* %anf_45bind48278, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52098, %struct.ScmObj* %_37foldl48129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52098, %struct.ScmObj* %_37foldr148047, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52098, %struct.ScmObj* %anf_45bind48286, i64 5)
%ae52099 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56101 = alloca %struct.ScmObj*, align 8
%fptrToInt56102 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52100 to i64
%ae52100 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56102)
store volatile %struct.ScmObj* %ae52100, %struct.ScmObj** %stackaddr$makeclosure56101, align 8
%argslist55088$ae520980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56103 = alloca %struct.ScmObj*, align 8
%argslist55088$ae520981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52100, %struct.ScmObj* %argslist55088$ae520980)
store volatile %struct.ScmObj* %argslist55088$ae520981, %struct.ScmObj** %stackaddr$prim56103, align 8
%stackaddr$prim56104 = alloca %struct.ScmObj*, align 8
%argslist55088$ae520982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52099, %struct.ScmObj* %argslist55088$ae520981)
store volatile %struct.ScmObj* %argslist55088$ae520982, %struct.ScmObj** %stackaddr$prim56104, align 8
%clofunc56105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52098)
musttail call tailcc void %clofunc56105(%struct.ScmObj* %ae52098, %struct.ScmObj* %argslist55088$ae520982)
ret void
}

define tailcc void @proc_clo$ae52098(%struct.ScmObj* %env$ae52098,%struct.ScmObj* %current_45args55037) {
%stackaddr$env-ref56106 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52098, i64 0)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref56106
%stackaddr$env-ref56107 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52098, i64 1)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref56107
%stackaddr$env-ref56108 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52098, i64 2)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref56108
%stackaddr$env-ref56109 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52098, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56109
%stackaddr$env-ref56110 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52098, i64 4)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56110
%stackaddr$env-ref56111 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52098, i64 5)
store %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$env-ref56111
%stackaddr$prim56112 = alloca %struct.ScmObj*, align 8
%_95k48340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55037)
store volatile %struct.ScmObj* %_95k48340, %struct.ScmObj** %stackaddr$prim56112, align 8
%stackaddr$prim56113 = alloca %struct.ScmObj*, align 8
%current_45args55038 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55037)
store volatile %struct.ScmObj* %current_45args55038, %struct.ScmObj** %stackaddr$prim56113, align 8
%stackaddr$prim56114 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55038)
store volatile %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$prim56114, align 8
%stackaddr$makeclosure56115 = alloca %struct.ScmObj*, align 8
%fptrToInt56116 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52120 to i64
%ae52120 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56116)
store volatile %struct.ScmObj* %ae52120, %struct.ScmObj** %stackaddr$makeclosure56115, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52120, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52120, %struct.ScmObj* %anf_45bind48287, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52120, %struct.ScmObj* %anf_45bind48278, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52120, %struct.ScmObj* %_37foldl48129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52120, %struct.ScmObj* %anf_45bind48286, i64 4)
%ae52121 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56117 = alloca %struct.ScmObj*, align 8
%fptrToInt56118 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52122 to i64
%ae52122 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56118)
store volatile %struct.ScmObj* %ae52122, %struct.ScmObj** %stackaddr$makeclosure56117, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52122, %struct.ScmObj* %_37first48089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52122, %struct.ScmObj* %_37second48087, i64 1)
%argslist55086$ae521200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56119 = alloca %struct.ScmObj*, align 8
%argslist55086$ae521201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52122, %struct.ScmObj* %argslist55086$ae521200)
store volatile %struct.ScmObj* %argslist55086$ae521201, %struct.ScmObj** %stackaddr$prim56119, align 8
%stackaddr$prim56120 = alloca %struct.ScmObj*, align 8
%argslist55086$ae521202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52121, %struct.ScmObj* %argslist55086$ae521201)
store volatile %struct.ScmObj* %argslist55086$ae521202, %struct.ScmObj** %stackaddr$prim56120, align 8
%clofunc56121 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52120)
musttail call tailcc void %clofunc56121(%struct.ScmObj* %ae52120, %struct.ScmObj* %argslist55086$ae521202)
ret void
}

define tailcc void @proc_clo$ae52120(%struct.ScmObj* %env$ae52120,%struct.ScmObj* %current_45args55040) {
%stackaddr$env-ref56122 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52120, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56122
%stackaddr$env-ref56123 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52120, i64 1)
store %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$env-ref56123
%stackaddr$env-ref56124 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52120, i64 2)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref56124
%stackaddr$env-ref56125 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52120, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56125
%stackaddr$env-ref56126 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52120, i64 4)
store %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$env-ref56126
%stackaddr$prim56127 = alloca %struct.ScmObj*, align 8
%_95k48341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55040)
store volatile %struct.ScmObj* %_95k48341, %struct.ScmObj** %stackaddr$prim56127, align 8
%stackaddr$prim56128 = alloca %struct.ScmObj*, align 8
%current_45args55041 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55040)
store volatile %struct.ScmObj* %current_45args55041, %struct.ScmObj** %stackaddr$prim56128, align 8
%stackaddr$prim56129 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55041)
store volatile %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$prim56129, align 8
%stackaddr$makeclosure56130 = alloca %struct.ScmObj*, align 8
%fptrToInt56131 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52192 to i64
%ae52192 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56131)
store volatile %struct.ScmObj* %ae52192, %struct.ScmObj** %stackaddr$makeclosure56130, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52192, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52192, %struct.ScmObj* %anf_45bind48287, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52192, %struct.ScmObj* %anf_45bind48278, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52192, %struct.ScmObj* %anf_45bind48292, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52192, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52192, %struct.ScmObj* %anf_45bind48286, i64 5)
%ae52193 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56132 = alloca %struct.ScmObj*, align 8
%fptrToInt56133 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52194 to i64
%ae52194 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56133)
store volatile %struct.ScmObj* %ae52194, %struct.ScmObj** %stackaddr$makeclosure56132, align 8
%argslist55072$ae521920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56134 = alloca %struct.ScmObj*, align 8
%argslist55072$ae521921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52194, %struct.ScmObj* %argslist55072$ae521920)
store volatile %struct.ScmObj* %argslist55072$ae521921, %struct.ScmObj** %stackaddr$prim56134, align 8
%stackaddr$prim56135 = alloca %struct.ScmObj*, align 8
%argslist55072$ae521922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52193, %struct.ScmObj* %argslist55072$ae521921)
store volatile %struct.ScmObj* %argslist55072$ae521922, %struct.ScmObj** %stackaddr$prim56135, align 8
%clofunc56136 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52192)
musttail call tailcc void %clofunc56136(%struct.ScmObj* %ae52192, %struct.ScmObj* %argslist55072$ae521922)
ret void
}

define tailcc void @proc_clo$ae52192(%struct.ScmObj* %env$ae52192,%struct.ScmObj* %current_45args55043) {
%stackaddr$env-ref56137 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52192, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56137
%stackaddr$env-ref56138 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52192, i64 1)
store %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$env-ref56138
%stackaddr$env-ref56139 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52192, i64 2)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref56139
%stackaddr$env-ref56140 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52192, i64 3)
store %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$env-ref56140
%stackaddr$env-ref56141 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52192, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56141
%stackaddr$env-ref56142 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52192, i64 5)
store %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$env-ref56142
%stackaddr$prim56143 = alloca %struct.ScmObj*, align 8
%_95k48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55043)
store volatile %struct.ScmObj* %_95k48342, %struct.ScmObj** %stackaddr$prim56143, align 8
%stackaddr$prim56144 = alloca %struct.ScmObj*, align 8
%current_45args55044 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55043)
store volatile %struct.ScmObj* %current_45args55044, %struct.ScmObj** %stackaddr$prim56144, align 8
%stackaddr$prim56145 = alloca %struct.ScmObj*, align 8
%anf_45bind48295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55044)
store volatile %struct.ScmObj* %anf_45bind48295, %struct.ScmObj** %stackaddr$prim56145, align 8
%stackaddr$makeclosure56146 = alloca %struct.ScmObj*, align 8
%fptrToInt56147 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52222 to i64
%ae52222 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56147)
store volatile %struct.ScmObj* %ae52222, %struct.ScmObj** %stackaddr$makeclosure56146, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52222, %struct.ScmObj* %anf_45bind48295, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52222, %struct.ScmObj* %anf_45bind48278, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52222, %struct.ScmObj* %anf_45bind48292, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52222, %struct.ScmObj* %_37foldl48129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52222, %struct.ScmObj* %anf_45bind48287, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52222, %struct.ScmObj* %anf_45bind48286, i64 5)
%ae52223 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56148 = alloca %struct.ScmObj*, align 8
%fptrToInt56149 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52224 to i64
%ae52224 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56149)
store volatile %struct.ScmObj* %ae52224, %struct.ScmObj** %stackaddr$makeclosure56148, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52224, %struct.ScmObj* %_37foldr148047, i64 0)
%argslist55064$ae522220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56150 = alloca %struct.ScmObj*, align 8
%argslist55064$ae522221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52224, %struct.ScmObj* %argslist55064$ae522220)
store volatile %struct.ScmObj* %argslist55064$ae522221, %struct.ScmObj** %stackaddr$prim56150, align 8
%stackaddr$prim56151 = alloca %struct.ScmObj*, align 8
%argslist55064$ae522222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52223, %struct.ScmObj* %argslist55064$ae522221)
store volatile %struct.ScmObj* %argslist55064$ae522222, %struct.ScmObj** %stackaddr$prim56151, align 8
%clofunc56152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52222)
musttail call tailcc void %clofunc56152(%struct.ScmObj* %ae52222, %struct.ScmObj* %argslist55064$ae522222)
ret void
}

define tailcc void @proc_clo$ae52222(%struct.ScmObj* %env$ae52222,%struct.ScmObj* %current_45args55046) {
%stackaddr$env-ref56153 = alloca %struct.ScmObj*, align 8
%anf_45bind48295 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52222, i64 0)
store %struct.ScmObj* %anf_45bind48295, %struct.ScmObj** %stackaddr$env-ref56153
%stackaddr$env-ref56154 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52222, i64 1)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref56154
%stackaddr$env-ref56155 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52222, i64 2)
store %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$env-ref56155
%stackaddr$env-ref56156 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52222, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56156
%stackaddr$env-ref56157 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52222, i64 4)
store %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$env-ref56157
%stackaddr$env-ref56158 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52222, i64 5)
store %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$env-ref56158
%stackaddr$prim56159 = alloca %struct.ScmObj*, align 8
%_95k48343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55046)
store volatile %struct.ScmObj* %_95k48343, %struct.ScmObj** %stackaddr$prim56159, align 8
%stackaddr$prim56160 = alloca %struct.ScmObj*, align 8
%current_45args55047 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55046)
store volatile %struct.ScmObj* %current_45args55047, %struct.ScmObj** %stackaddr$prim56160, align 8
%stackaddr$prim56161 = alloca %struct.ScmObj*, align 8
%anf_45bind48297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55047)
store volatile %struct.ScmObj* %anf_45bind48297, %struct.ScmObj** %stackaddr$prim56161, align 8
%stackaddr$makeclosure56162 = alloca %struct.ScmObj*, align 8
%fptrToInt56163 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52293 to i64
%ae52293 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56163)
store volatile %struct.ScmObj* %ae52293, %struct.ScmObj** %stackaddr$makeclosure56162, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52293, %struct.ScmObj* %anf_45bind48278, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52293, %struct.ScmObj* %_37foldl48129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52293, %struct.ScmObj* %anf_45bind48286, i64 2)
%argslist55057$anf_45bind482870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56164 = alloca %struct.ScmObj*, align 8
%argslist55057$anf_45bind482871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48297, %struct.ScmObj* %argslist55057$anf_45bind482870)
store volatile %struct.ScmObj* %argslist55057$anf_45bind482871, %struct.ScmObj** %stackaddr$prim56164, align 8
%stackaddr$prim56165 = alloca %struct.ScmObj*, align 8
%argslist55057$anf_45bind482872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48295, %struct.ScmObj* %argslist55057$anf_45bind482871)
store volatile %struct.ScmObj* %argslist55057$anf_45bind482872, %struct.ScmObj** %stackaddr$prim56165, align 8
%stackaddr$prim56166 = alloca %struct.ScmObj*, align 8
%argslist55057$anf_45bind482873 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48292, %struct.ScmObj* %argslist55057$anf_45bind482872)
store volatile %struct.ScmObj* %argslist55057$anf_45bind482873, %struct.ScmObj** %stackaddr$prim56166, align 8
%stackaddr$prim56167 = alloca %struct.ScmObj*, align 8
%argslist55057$anf_45bind482874 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52293, %struct.ScmObj* %argslist55057$anf_45bind482873)
store volatile %struct.ScmObj* %argslist55057$anf_45bind482874, %struct.ScmObj** %stackaddr$prim56167, align 8
%clofunc56168 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48287)
musttail call tailcc void %clofunc56168(%struct.ScmObj* %anf_45bind48287, %struct.ScmObj* %argslist55057$anf_45bind482874)
ret void
}

define tailcc void @proc_clo$ae52293(%struct.ScmObj* %env$ae52293,%struct.ScmObj* %current_45args55049) {
%stackaddr$env-ref56169 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52293, i64 0)
store %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$env-ref56169
%stackaddr$env-ref56170 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52293, i64 1)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56170
%stackaddr$env-ref56171 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52293, i64 2)
store %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$env-ref56171
%stackaddr$prim56172 = alloca %struct.ScmObj*, align 8
%_95k48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55049)
store volatile %struct.ScmObj* %_95k48344, %struct.ScmObj** %stackaddr$prim56172, align 8
%stackaddr$prim56173 = alloca %struct.ScmObj*, align 8
%current_45args55050 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55049)
store volatile %struct.ScmObj* %current_45args55050, %struct.ScmObj** %stackaddr$prim56173, align 8
%stackaddr$prim56174 = alloca %struct.ScmObj*, align 8
%anf_45bind48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55050)
store volatile %struct.ScmObj* %anf_45bind48298, %struct.ScmObj** %stackaddr$prim56174, align 8
%stackaddr$makeclosure56175 = alloca %struct.ScmObj*, align 8
%fptrToInt56176 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52298 to i64
%ae52298 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56176)
store volatile %struct.ScmObj* %ae52298, %struct.ScmObj** %stackaddr$makeclosure56175, align 8
%ae52300 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55056$_37foldl481290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56177 = alloca %struct.ScmObj*, align 8
%argslist55056$_37foldl481291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48298, %struct.ScmObj* %argslist55056$_37foldl481290)
store volatile %struct.ScmObj* %argslist55056$_37foldl481291, %struct.ScmObj** %stackaddr$prim56177, align 8
%stackaddr$prim56178 = alloca %struct.ScmObj*, align 8
%argslist55056$_37foldl481292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48286, %struct.ScmObj* %argslist55056$_37foldl481291)
store volatile %struct.ScmObj* %argslist55056$_37foldl481292, %struct.ScmObj** %stackaddr$prim56178, align 8
%stackaddr$prim56179 = alloca %struct.ScmObj*, align 8
%argslist55056$_37foldl481293 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52300, %struct.ScmObj* %argslist55056$_37foldl481292)
store volatile %struct.ScmObj* %argslist55056$_37foldl481293, %struct.ScmObj** %stackaddr$prim56179, align 8
%stackaddr$prim56180 = alloca %struct.ScmObj*, align 8
%argslist55056$_37foldl481294 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48278, %struct.ScmObj* %argslist55056$_37foldl481293)
store volatile %struct.ScmObj* %argslist55056$_37foldl481294, %struct.ScmObj** %stackaddr$prim56180, align 8
%stackaddr$prim56181 = alloca %struct.ScmObj*, align 8
%argslist55056$_37foldl481295 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52298, %struct.ScmObj* %argslist55056$_37foldl481294)
store volatile %struct.ScmObj* %argslist55056$_37foldl481295, %struct.ScmObj** %stackaddr$prim56181, align 8
%clofunc56182 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48129)
musttail call tailcc void %clofunc56182(%struct.ScmObj* %_37foldl48129, %struct.ScmObj* %argslist55056$_37foldl481295)
ret void
}

define tailcc void @proc_clo$ae52298(%struct.ScmObj* %env$ae52298,%struct.ScmObj* %current_45args55052) {
%stackaddr$prim56183 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55052)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim56183, align 8
%stackaddr$prim56184 = alloca %struct.ScmObj*, align 8
%current_45args55053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55052)
store volatile %struct.ScmObj* %current_45args55053, %struct.ScmObj** %stackaddr$prim56184, align 8
%stackaddr$prim56185 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55053)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim56185, align 8
%stackaddr$prim56186 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim56186, align 8
%argslist55055$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56187 = alloca %struct.ScmObj*, align 8
%argslist55055$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist55055$k0)
store volatile %struct.ScmObj* %argslist55055$k1, %struct.ScmObj** %stackaddr$prim56187, align 8
%clofunc56188 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc56188(%struct.ScmObj* %k, %struct.ScmObj* %argslist55055$k1)
ret void
}

define tailcc void @proc_clo$ae52224(%struct.ScmObj* %env$ae52224,%struct.ScmObj* %a4816248345) {
%stackaddr$env-ref56189 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52224, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56189
%stackaddr$prim56190 = alloca %struct.ScmObj*, align 8
%k48346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %a4816248345)
store volatile %struct.ScmObj* %k48346, %struct.ScmObj** %stackaddr$prim56190, align 8
%stackaddr$prim56191 = alloca %struct.ScmObj*, align 8
%a48162 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %a4816248345)
store volatile %struct.ScmObj* %a48162, %struct.ScmObj** %stackaddr$prim56191, align 8
%stackaddr$makeclosure56192 = alloca %struct.ScmObj*, align 8
%fptrToInt56193 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52227 to i64
%ae52227 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56193)
store volatile %struct.ScmObj* %ae52227, %struct.ScmObj** %stackaddr$makeclosure56192, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52227, %struct.ScmObj* %a48162, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52227, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52227, %struct.ScmObj* %k48346, i64 2)
%ae52228 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56194 = alloca %struct.ScmObj*, align 8
%fptrToInt56195 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52229 to i64
%ae52229 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56195)
store volatile %struct.ScmObj* %ae52229, %struct.ScmObj** %stackaddr$makeclosure56194, align 8
%argslist55063$ae522270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56196 = alloca %struct.ScmObj*, align 8
%argslist55063$ae522271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52229, %struct.ScmObj* %argslist55063$ae522270)
store volatile %struct.ScmObj* %argslist55063$ae522271, %struct.ScmObj** %stackaddr$prim56196, align 8
%stackaddr$prim56197 = alloca %struct.ScmObj*, align 8
%argslist55063$ae522272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52228, %struct.ScmObj* %argslist55063$ae522271)
store volatile %struct.ScmObj* %argslist55063$ae522272, %struct.ScmObj** %stackaddr$prim56197, align 8
%clofunc56198 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52227)
musttail call tailcc void %clofunc56198(%struct.ScmObj* %ae52227, %struct.ScmObj* %argslist55063$ae522272)
ret void
}

define tailcc void @proc_clo$ae52227(%struct.ScmObj* %env$ae52227,%struct.ScmObj* %current_45args55058) {
%stackaddr$env-ref56199 = alloca %struct.ScmObj*, align 8
%a48162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52227, i64 0)
store %struct.ScmObj* %a48162, %struct.ScmObj** %stackaddr$env-ref56199
%stackaddr$env-ref56200 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52227, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56200
%stackaddr$env-ref56201 = alloca %struct.ScmObj*, align 8
%k48346 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52227, i64 2)
store %struct.ScmObj* %k48346, %struct.ScmObj** %stackaddr$env-ref56201
%stackaddr$prim56202 = alloca %struct.ScmObj*, align 8
%_95k48347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55058)
store volatile %struct.ScmObj* %_95k48347, %struct.ScmObj** %stackaddr$prim56202, align 8
%stackaddr$prim56203 = alloca %struct.ScmObj*, align 8
%current_45args55059 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55058)
store volatile %struct.ScmObj* %current_45args55059, %struct.ScmObj** %stackaddr$prim56203, align 8
%stackaddr$prim56204 = alloca %struct.ScmObj*, align 8
%anf_45bind48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55059)
store volatile %struct.ScmObj* %anf_45bind48296, %struct.ScmObj** %stackaddr$prim56204, align 8
%ae52254 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55061$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56205 = alloca %struct.ScmObj*, align 8
%argslist55061$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48162, %struct.ScmObj* %argslist55061$_37foldr1480470)
store volatile %struct.ScmObj* %argslist55061$_37foldr1480471, %struct.ScmObj** %stackaddr$prim56205, align 8
%stackaddr$prim56206 = alloca %struct.ScmObj*, align 8
%argslist55061$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52254, %struct.ScmObj* %argslist55061$_37foldr1480471)
store volatile %struct.ScmObj* %argslist55061$_37foldr1480472, %struct.ScmObj** %stackaddr$prim56206, align 8
%stackaddr$prim56207 = alloca %struct.ScmObj*, align 8
%argslist55061$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48296, %struct.ScmObj* %argslist55061$_37foldr1480472)
store volatile %struct.ScmObj* %argslist55061$_37foldr1480473, %struct.ScmObj** %stackaddr$prim56207, align 8
%stackaddr$prim56208 = alloca %struct.ScmObj*, align 8
%argslist55061$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48346, %struct.ScmObj* %argslist55061$_37foldr1480473)
store volatile %struct.ScmObj* %argslist55061$_37foldr1480474, %struct.ScmObj** %stackaddr$prim56208, align 8
%clofunc56209 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc56209(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist55061$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae52229(%struct.ScmObj* %env$ae52229,%struct.ScmObj* %args4816348348) {
%stackaddr$prim56210 = alloca %struct.ScmObj*, align 8
%k48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4816348348)
store volatile %struct.ScmObj* %k48349, %struct.ScmObj** %stackaddr$prim56210, align 8
%stackaddr$prim56211 = alloca %struct.ScmObj*, align 8
%args48163 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4816348348)
store volatile %struct.ScmObj* %args48163, %struct.ScmObj** %stackaddr$prim56211, align 8
%stackaddr$applyprim56212 = alloca %struct.ScmObj*, align 8
%cpsaprim48350 = call %struct.ScmObj* @applyprim__43(%struct.ScmObj* %args48163)
store volatile %struct.ScmObj* %cpsaprim48350, %struct.ScmObj** %stackaddr$applyprim56212, align 8
%ae52234 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55062$k483490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56213 = alloca %struct.ScmObj*, align 8
%argslist55062$k483491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim48350, %struct.ScmObj* %argslist55062$k483490)
store volatile %struct.ScmObj* %argslist55062$k483491, %struct.ScmObj** %stackaddr$prim56213, align 8
%stackaddr$prim56214 = alloca %struct.ScmObj*, align 8
%argslist55062$k483492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52234, %struct.ScmObj* %argslist55062$k483491)
store volatile %struct.ScmObj* %argslist55062$k483492, %struct.ScmObj** %stackaddr$prim56214, align 8
%clofunc56215 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48349)
musttail call tailcc void %clofunc56215(%struct.ScmObj* %k48349, %struct.ScmObj* %argslist55062$k483492)
ret void
}

define tailcc void @proc_clo$ae52194(%struct.ScmObj* %env$ae52194,%struct.ScmObj* %current_45args55065) {
%stackaddr$prim56216 = alloca %struct.ScmObj*, align 8
%k48351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55065)
store volatile %struct.ScmObj* %k48351, %struct.ScmObj** %stackaddr$prim56216, align 8
%stackaddr$prim56217 = alloca %struct.ScmObj*, align 8
%current_45args55066 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55065)
store volatile %struct.ScmObj* %current_45args55066, %struct.ScmObj** %stackaddr$prim56217, align 8
%stackaddr$prim56218 = alloca %struct.ScmObj*, align 8
%a48161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55066)
store volatile %struct.ScmObj* %a48161, %struct.ScmObj** %stackaddr$prim56218, align 8
%stackaddr$prim56219 = alloca %struct.ScmObj*, align 8
%current_45args55067 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55066)
store volatile %struct.ScmObj* %current_45args55067, %struct.ScmObj** %stackaddr$prim56219, align 8
%stackaddr$prim56220 = alloca %struct.ScmObj*, align 8
%b48160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55067)
store volatile %struct.ScmObj* %b48160, %struct.ScmObj** %stackaddr$prim56220, align 8
%stackaddr$prim56221 = alloca %struct.ScmObj*, align 8
%current_45args55068 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55067)
store volatile %struct.ScmObj* %current_45args55068, %struct.ScmObj** %stackaddr$prim56221, align 8
%stackaddr$prim56222 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55068)
store volatile %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$prim56222, align 8
%stackaddr$prim56223 = alloca %struct.ScmObj*, align 8
%current_45args55069 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55068)
store volatile %struct.ScmObj* %current_45args55069, %struct.ScmObj** %stackaddr$prim56223, align 8
%stackaddr$prim56224 = alloca %struct.ScmObj*, align 8
%d48158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55069)
store volatile %struct.ScmObj* %d48158, %struct.ScmObj** %stackaddr$prim56224, align 8
%stackaddr$prim56225 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %a48161, %struct.ScmObj* %b48160)
store volatile %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$prim56225, align 8
%stackaddr$prim56226 = alloca %struct.ScmObj*, align 8
%anf_45bind48294 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %c48159, %struct.ScmObj* %d48158)
store volatile %struct.ScmObj* %anf_45bind48294, %struct.ScmObj** %stackaddr$prim56226, align 8
%stackaddr$prim56227 = alloca %struct.ScmObj*, align 8
%cpsprim48352 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind48293, %struct.ScmObj* %anf_45bind48294)
store volatile %struct.ScmObj* %cpsprim48352, %struct.ScmObj** %stackaddr$prim56227, align 8
%ae52202 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55071$k483510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56228 = alloca %struct.ScmObj*, align 8
%argslist55071$k483511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48352, %struct.ScmObj* %argslist55071$k483510)
store volatile %struct.ScmObj* %argslist55071$k483511, %struct.ScmObj** %stackaddr$prim56228, align 8
%stackaddr$prim56229 = alloca %struct.ScmObj*, align 8
%argslist55071$k483512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52202, %struct.ScmObj* %argslist55071$k483511)
store volatile %struct.ScmObj* %argslist55071$k483512, %struct.ScmObj** %stackaddr$prim56229, align 8
%clofunc56230 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48351)
musttail call tailcc void %clofunc56230(%struct.ScmObj* %k48351, %struct.ScmObj* %argslist55071$k483512)
ret void
}

define tailcc void @proc_clo$ae52122(%struct.ScmObj* %env$ae52122,%struct.ScmObj* %t480254815248353) {
%stackaddr$env-ref56231 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52122, i64 0)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref56231
%stackaddr$env-ref56232 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52122, i64 1)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref56232
%stackaddr$prim56233 = alloca %struct.ScmObj*, align 8
%k48354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t480254815248353)
store volatile %struct.ScmObj* %k48354, %struct.ScmObj** %stackaddr$prim56233, align 8
%stackaddr$prim56234 = alloca %struct.ScmObj*, align 8
%t4802548152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t480254815248353)
store volatile %struct.ScmObj* %t4802548152, %struct.ScmObj** %stackaddr$prim56234, align 8
%stackaddr$prim56235 = alloca %struct.ScmObj*, align 8
%a48153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t4802548152)
store volatile %struct.ScmObj* %a48153, %struct.ScmObj** %stackaddr$prim56235, align 8
%stackaddr$prim56236 = alloca %struct.ScmObj*, align 8
%t4802548154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t4802548152)
store volatile %struct.ScmObj* %t4802548154, %struct.ScmObj** %stackaddr$prim56236, align 8
%stackaddr$prim56237 = alloca %struct.ScmObj*, align 8
%b48155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t4802548154)
store volatile %struct.ScmObj* %b48155, %struct.ScmObj** %stackaddr$prim56237, align 8
%stackaddr$prim56238 = alloca %struct.ScmObj*, align 8
%t4802548156 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t4802548154)
store volatile %struct.ScmObj* %t4802548156, %struct.ScmObj** %stackaddr$prim56238, align 8
%stackaddr$makeclosure56239 = alloca %struct.ScmObj*, align 8
%fptrToInt56240 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52129 to i64
%ae52129 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56240)
store volatile %struct.ScmObj* %ae52129, %struct.ScmObj** %stackaddr$makeclosure56239, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52129, %struct.ScmObj* %a48153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52129, %struct.ScmObj* %_37first48089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52129, %struct.ScmObj* %_37second48087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52129, %struct.ScmObj* %k48354, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52129, %struct.ScmObj* %b48155, i64 4)
%ae52130 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55085$ae521290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56241 = alloca %struct.ScmObj*, align 8
%argslist55085$ae521291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %t4802548156, %struct.ScmObj* %argslist55085$ae521290)
store volatile %struct.ScmObj* %argslist55085$ae521291, %struct.ScmObj** %stackaddr$prim56241, align 8
%stackaddr$prim56242 = alloca %struct.ScmObj*, align 8
%argslist55085$ae521292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52130, %struct.ScmObj* %argslist55085$ae521291)
store volatile %struct.ScmObj* %argslist55085$ae521292, %struct.ScmObj** %stackaddr$prim56242, align 8
%clofunc56243 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52129)
musttail call tailcc void %clofunc56243(%struct.ScmObj* %ae52129, %struct.ScmObj* %argslist55085$ae521292)
ret void
}

define tailcc void @proc_clo$ae52129(%struct.ScmObj* %env$ae52129,%struct.ScmObj* %current_45args55073) {
%stackaddr$env-ref56244 = alloca %struct.ScmObj*, align 8
%a48153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52129, i64 0)
store %struct.ScmObj* %a48153, %struct.ScmObj** %stackaddr$env-ref56244
%stackaddr$env-ref56245 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52129, i64 1)
store %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$env-ref56245
%stackaddr$env-ref56246 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52129, i64 2)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref56246
%stackaddr$env-ref56247 = alloca %struct.ScmObj*, align 8
%k48354 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52129, i64 3)
store %struct.ScmObj* %k48354, %struct.ScmObj** %stackaddr$env-ref56247
%stackaddr$env-ref56248 = alloca %struct.ScmObj*, align 8
%b48155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52129, i64 4)
store %struct.ScmObj* %b48155, %struct.ScmObj** %stackaddr$env-ref56248
%stackaddr$prim56249 = alloca %struct.ScmObj*, align 8
%_95k48355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55073)
store volatile %struct.ScmObj* %_95k48355, %struct.ScmObj** %stackaddr$prim56249, align 8
%stackaddr$prim56250 = alloca %struct.ScmObj*, align 8
%current_45args55074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55073)
store volatile %struct.ScmObj* %current_45args55074, %struct.ScmObj** %stackaddr$prim56250, align 8
%stackaddr$prim56251 = alloca %struct.ScmObj*, align 8
%c48157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55074)
store volatile %struct.ScmObj* %c48157, %struct.ScmObj** %stackaddr$prim56251, align 8
%stackaddr$makeclosure56252 = alloca %struct.ScmObj*, align 8
%fptrToInt56253 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52136 to i64
%ae52136 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56253)
store volatile %struct.ScmObj* %ae52136, %struct.ScmObj** %stackaddr$makeclosure56252, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52136, %struct.ScmObj* %a48153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52136, %struct.ScmObj* %_37second48087, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52136, %struct.ScmObj* %k48354, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52136, %struct.ScmObj* %c48157, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52136, %struct.ScmObj* %b48155, i64 4)
%argslist55084$_37first480890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56254 = alloca %struct.ScmObj*, align 8
%argslist55084$_37first480891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c48157, %struct.ScmObj* %argslist55084$_37first480890)
store volatile %struct.ScmObj* %argslist55084$_37first480891, %struct.ScmObj** %stackaddr$prim56254, align 8
%stackaddr$prim56255 = alloca %struct.ScmObj*, align 8
%argslist55084$_37first480892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52136, %struct.ScmObj* %argslist55084$_37first480891)
store volatile %struct.ScmObj* %argslist55084$_37first480892, %struct.ScmObj** %stackaddr$prim56255, align 8
%clofunc56256 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37first48089)
musttail call tailcc void %clofunc56256(%struct.ScmObj* %_37first48089, %struct.ScmObj* %argslist55084$_37first480892)
ret void
}

define tailcc void @proc_clo$ae52136(%struct.ScmObj* %env$ae52136,%struct.ScmObj* %current_45args55076) {
%stackaddr$env-ref56257 = alloca %struct.ScmObj*, align 8
%a48153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52136, i64 0)
store %struct.ScmObj* %a48153, %struct.ScmObj** %stackaddr$env-ref56257
%stackaddr$env-ref56258 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52136, i64 1)
store %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$env-ref56258
%stackaddr$env-ref56259 = alloca %struct.ScmObj*, align 8
%k48354 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52136, i64 2)
store %struct.ScmObj* %k48354, %struct.ScmObj** %stackaddr$env-ref56259
%stackaddr$env-ref56260 = alloca %struct.ScmObj*, align 8
%c48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52136, i64 3)
store %struct.ScmObj* %c48157, %struct.ScmObj** %stackaddr$env-ref56260
%stackaddr$env-ref56261 = alloca %struct.ScmObj*, align 8
%b48155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52136, i64 4)
store %struct.ScmObj* %b48155, %struct.ScmObj** %stackaddr$env-ref56261
%stackaddr$prim56262 = alloca %struct.ScmObj*, align 8
%_95k48356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55076)
store volatile %struct.ScmObj* %_95k48356, %struct.ScmObj** %stackaddr$prim56262, align 8
%stackaddr$prim56263 = alloca %struct.ScmObj*, align 8
%current_45args55077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55076)
store volatile %struct.ScmObj* %current_45args55077, %struct.ScmObj** %stackaddr$prim56263, align 8
%stackaddr$prim56264 = alloca %struct.ScmObj*, align 8
%anf_45bind48288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55077)
store volatile %struct.ScmObj* %anf_45bind48288, %struct.ScmObj** %stackaddr$prim56264, align 8
%stackaddr$makeclosure56265 = alloca %struct.ScmObj*, align 8
%fptrToInt56266 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52139 to i64
%ae52139 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56266)
store volatile %struct.ScmObj* %ae52139, %struct.ScmObj** %stackaddr$makeclosure56265, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52139, %struct.ScmObj* %a48153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52139, %struct.ScmObj* %k48354, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52139, %struct.ScmObj* %anf_45bind48288, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52139, %struct.ScmObj* %b48155, i64 3)
%argslist55083$_37second480870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56267 = alloca %struct.ScmObj*, align 8
%argslist55083$_37second480871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c48157, %struct.ScmObj* %argslist55083$_37second480870)
store volatile %struct.ScmObj* %argslist55083$_37second480871, %struct.ScmObj** %stackaddr$prim56267, align 8
%stackaddr$prim56268 = alloca %struct.ScmObj*, align 8
%argslist55083$_37second480872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52139, %struct.ScmObj* %argslist55083$_37second480871)
store volatile %struct.ScmObj* %argslist55083$_37second480872, %struct.ScmObj** %stackaddr$prim56268, align 8
%clofunc56269 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37second48087)
musttail call tailcc void %clofunc56269(%struct.ScmObj* %_37second48087, %struct.ScmObj* %argslist55083$_37second480872)
ret void
}

define tailcc void @proc_clo$ae52139(%struct.ScmObj* %env$ae52139,%struct.ScmObj* %current_45args55079) {
%stackaddr$env-ref56270 = alloca %struct.ScmObj*, align 8
%a48153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52139, i64 0)
store %struct.ScmObj* %a48153, %struct.ScmObj** %stackaddr$env-ref56270
%stackaddr$env-ref56271 = alloca %struct.ScmObj*, align 8
%k48354 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52139, i64 1)
store %struct.ScmObj* %k48354, %struct.ScmObj** %stackaddr$env-ref56271
%stackaddr$env-ref56272 = alloca %struct.ScmObj*, align 8
%anf_45bind48288 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52139, i64 2)
store %struct.ScmObj* %anf_45bind48288, %struct.ScmObj** %stackaddr$env-ref56272
%stackaddr$env-ref56273 = alloca %struct.ScmObj*, align 8
%b48155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52139, i64 3)
store %struct.ScmObj* %b48155, %struct.ScmObj** %stackaddr$env-ref56273
%stackaddr$prim56274 = alloca %struct.ScmObj*, align 8
%_95k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55079)
store volatile %struct.ScmObj* %_95k48357, %struct.ScmObj** %stackaddr$prim56274, align 8
%stackaddr$prim56275 = alloca %struct.ScmObj*, align 8
%current_45args55080 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55079)
store volatile %struct.ScmObj* %current_45args55080, %struct.ScmObj** %stackaddr$prim56275, align 8
%stackaddr$prim56276 = alloca %struct.ScmObj*, align 8
%anf_45bind48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55080)
store volatile %struct.ScmObj* %anf_45bind48289, %struct.ScmObj** %stackaddr$prim56276, align 8
%stackaddr$prim56277 = alloca %struct.ScmObj*, align 8
%anf_45bind48290 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind48288, %struct.ScmObj* %anf_45bind48289)
store volatile %struct.ScmObj* %anf_45bind48290, %struct.ScmObj** %stackaddr$prim56277, align 8
%stackaddr$prim56278 = alloca %struct.ScmObj*, align 8
%anf_45bind48291 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %b48155, %struct.ScmObj* %anf_45bind48290)
store volatile %struct.ScmObj* %anf_45bind48291, %struct.ScmObj** %stackaddr$prim56278, align 8
%stackaddr$prim56279 = alloca %struct.ScmObj*, align 8
%cpsprim48358 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %a48153, %struct.ScmObj* %anf_45bind48291)
store volatile %struct.ScmObj* %cpsprim48358, %struct.ScmObj** %stackaddr$prim56279, align 8
%ae52148 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55082$k483540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56280 = alloca %struct.ScmObj*, align 8
%argslist55082$k483541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48358, %struct.ScmObj* %argslist55082$k483540)
store volatile %struct.ScmObj* %argslist55082$k483541, %struct.ScmObj** %stackaddr$prim56280, align 8
%stackaddr$prim56281 = alloca %struct.ScmObj*, align 8
%argslist55082$k483542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52148, %struct.ScmObj* %argslist55082$k483541)
store volatile %struct.ScmObj* %argslist55082$k483542, %struct.ScmObj** %stackaddr$prim56281, align 8
%clofunc56282 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48354)
musttail call tailcc void %clofunc56282(%struct.ScmObj* %k48354, %struct.ScmObj* %argslist55082$k483542)
ret void
}

define tailcc void @proc_clo$ae52100(%struct.ScmObj* %env$ae52100,%struct.ScmObj* %a4815148359) {
%stackaddr$prim56283 = alloca %struct.ScmObj*, align 8
%k48360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %a4815148359)
store volatile %struct.ScmObj* %k48360, %struct.ScmObj** %stackaddr$prim56283, align 8
%stackaddr$prim56284 = alloca %struct.ScmObj*, align 8
%a48151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %a4815148359)
store volatile %struct.ScmObj* %a48151, %struct.ScmObj** %stackaddr$prim56284, align 8
%ae52104 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55087$k483600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56285 = alloca %struct.ScmObj*, align 8
%argslist55087$k483601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48151, %struct.ScmObj* %argslist55087$k483600)
store volatile %struct.ScmObj* %argslist55087$k483601, %struct.ScmObj** %stackaddr$prim56285, align 8
%stackaddr$prim56286 = alloca %struct.ScmObj*, align 8
%argslist55087$k483602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52104, %struct.ScmObj* %argslist55087$k483601)
store volatile %struct.ScmObj* %argslist55087$k483602, %struct.ScmObj** %stackaddr$prim56286, align 8
%clofunc56287 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48360)
musttail call tailcc void %clofunc56287(%struct.ScmObj* %k48360, %struct.ScmObj* %argslist55087$k483602)
ret void
}

define tailcc void @proc_clo$ae52043(%struct.ScmObj* %env$ae52043,%struct.ScmObj* %lst4815048361) {
%stackaddr$prim56288 = alloca %struct.ScmObj*, align 8
%k48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4815048361)
store volatile %struct.ScmObj* %k48362, %struct.ScmObj** %stackaddr$prim56288, align 8
%stackaddr$prim56289 = alloca %struct.ScmObj*, align 8
%lst48150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4815048361)
store volatile %struct.ScmObj* %lst48150, %struct.ScmObj** %stackaddr$prim56289, align 8
%ae52047 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55091$k483620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56290 = alloca %struct.ScmObj*, align 8
%argslist55091$k483621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48150, %struct.ScmObj* %argslist55091$k483620)
store volatile %struct.ScmObj* %argslist55091$k483621, %struct.ScmObj** %stackaddr$prim56290, align 8
%stackaddr$prim56291 = alloca %struct.ScmObj*, align 8
%argslist55091$k483622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52047, %struct.ScmObj* %argslist55091$k483621)
store volatile %struct.ScmObj* %argslist55091$k483622, %struct.ScmObj** %stackaddr$prim56291, align 8
%clofunc56292 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48362)
musttail call tailcc void %clofunc56292(%struct.ScmObj* %k48362, %struct.ScmObj* %argslist55091$k483622)
ret void
}

define tailcc void @proc_clo$ae51991(%struct.ScmObj* %env$ae51991,%struct.ScmObj* %lst4814948363) {
%stackaddr$prim56293 = alloca %struct.ScmObj*, align 8
%k48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814948363)
store volatile %struct.ScmObj* %k48364, %struct.ScmObj** %stackaddr$prim56293, align 8
%stackaddr$prim56294 = alloca %struct.ScmObj*, align 8
%lst48149 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814948363)
store volatile %struct.ScmObj* %lst48149, %struct.ScmObj** %stackaddr$prim56294, align 8
%ae51995 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55094$k483640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56295 = alloca %struct.ScmObj*, align 8
%argslist55094$k483641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48149, %struct.ScmObj* %argslist55094$k483640)
store volatile %struct.ScmObj* %argslist55094$k483641, %struct.ScmObj** %stackaddr$prim56295, align 8
%stackaddr$prim56296 = alloca %struct.ScmObj*, align 8
%argslist55094$k483642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51995, %struct.ScmObj* %argslist55094$k483641)
store volatile %struct.ScmObj* %argslist55094$k483642, %struct.ScmObj** %stackaddr$prim56296, align 8
%clofunc56297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48364)
musttail call tailcc void %clofunc56297(%struct.ScmObj* %k48364, %struct.ScmObj* %argslist55094$k483642)
ret void
}

define tailcc void @proc_clo$ae51939(%struct.ScmObj* %env$ae51939,%struct.ScmObj* %lst4814848365) {
%stackaddr$prim56298 = alloca %struct.ScmObj*, align 8
%k48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814848365)
store volatile %struct.ScmObj* %k48366, %struct.ScmObj** %stackaddr$prim56298, align 8
%stackaddr$prim56299 = alloca %struct.ScmObj*, align 8
%lst48148 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814848365)
store volatile %struct.ScmObj* %lst48148, %struct.ScmObj** %stackaddr$prim56299, align 8
%ae51943 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55097$k483660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56300 = alloca %struct.ScmObj*, align 8
%argslist55097$k483661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48148, %struct.ScmObj* %argslist55097$k483660)
store volatile %struct.ScmObj* %argslist55097$k483661, %struct.ScmObj** %stackaddr$prim56300, align 8
%stackaddr$prim56301 = alloca %struct.ScmObj*, align 8
%argslist55097$k483662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51943, %struct.ScmObj* %argslist55097$k483661)
store volatile %struct.ScmObj* %argslist55097$k483662, %struct.ScmObj** %stackaddr$prim56301, align 8
%clofunc56302 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48366)
musttail call tailcc void %clofunc56302(%struct.ScmObj* %k48366, %struct.ScmObj* %argslist55097$k483662)
ret void
}

define tailcc void @proc_clo$ae51917(%struct.ScmObj* %env$ae51917,%struct.ScmObj* %lst4814748367) {
%stackaddr$prim56303 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814748367)
store volatile %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$prim56303, align 8
%stackaddr$prim56304 = alloca %struct.ScmObj*, align 8
%lst48147 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814748367)
store volatile %struct.ScmObj* %lst48147, %struct.ScmObj** %stackaddr$prim56304, align 8
%ae51921 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55099$k483680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56305 = alloca %struct.ScmObj*, align 8
%argslist55099$k483681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48147, %struct.ScmObj* %argslist55099$k483680)
store volatile %struct.ScmObj* %argslist55099$k483681, %struct.ScmObj** %stackaddr$prim56305, align 8
%stackaddr$prim56306 = alloca %struct.ScmObj*, align 8
%argslist55099$k483682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51921, %struct.ScmObj* %argslist55099$k483681)
store volatile %struct.ScmObj* %argslist55099$k483682, %struct.ScmObj** %stackaddr$prim56306, align 8
%clofunc56307 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48368)
musttail call tailcc void %clofunc56307(%struct.ScmObj* %k48368, %struct.ScmObj* %argslist55099$k483682)
ret void
}

define tailcc void @proc_clo$ae51879(%struct.ScmObj* %env$ae51879,%struct.ScmObj* %current_45args55101) {
%stackaddr$prim56308 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55101)
store volatile %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$prim56308, align 8
%stackaddr$prim56309 = alloca %struct.ScmObj*, align 8
%current_45args55102 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55101)
store volatile %struct.ScmObj* %current_45args55102, %struct.ScmObj** %stackaddr$prim56309, align 8
%stackaddr$prim56310 = alloca %struct.ScmObj*, align 8
%lst48146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55102)
store volatile %struct.ScmObj* %lst48146, %struct.ScmObj** %stackaddr$prim56310, align 8
%stackaddr$prim56311 = alloca %struct.ScmObj*, align 8
%current_45args55103 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55102)
store volatile %struct.ScmObj* %current_45args55103, %struct.ScmObj** %stackaddr$prim56311, align 8
%stackaddr$prim56312 = alloca %struct.ScmObj*, align 8
%f48145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55103)
store volatile %struct.ScmObj* %f48145, %struct.ScmObj** %stackaddr$prim56312, align 8
%stackaddr$prim56313 = alloca %struct.ScmObj*, align 8
%current_45args55104 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55103)
store volatile %struct.ScmObj* %current_45args55104, %struct.ScmObj** %stackaddr$prim56313, align 8
%stackaddr$prim56314 = alloca %struct.ScmObj*, align 8
%a48144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55104)
store volatile %struct.ScmObj* %a48144, %struct.ScmObj** %stackaddr$prim56314, align 8
%stackaddr$makeclosure56315 = alloca %struct.ScmObj*, align 8
%fptrToInt56316 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51880 to i64
%ae51880 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56316)
store volatile %struct.ScmObj* %ae51880, %struct.ScmObj** %stackaddr$makeclosure56315, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51880, %struct.ScmObj* %lst48146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51880, %struct.ScmObj* %k48369, i64 1)
%stackaddr$prim56317 = alloca %struct.ScmObj*, align 8
%cpsargs48372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51880, %struct.ScmObj* %lst48146)
store volatile %struct.ScmObj* %cpsargs48372, %struct.ScmObj** %stackaddr$prim56317, align 8
%clofunc56318 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48145)
musttail call tailcc void %clofunc56318(%struct.ScmObj* %f48145, %struct.ScmObj* %cpsargs48372)
ret void
}

define tailcc void @proc_clo$ae51880(%struct.ScmObj* %env$ae51880,%struct.ScmObj* %current_45args55106) {
%stackaddr$env-ref56319 = alloca %struct.ScmObj*, align 8
%lst48146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51880, i64 0)
store %struct.ScmObj* %lst48146, %struct.ScmObj** %stackaddr$env-ref56319
%stackaddr$env-ref56320 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51880, i64 1)
store %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$env-ref56320
%stackaddr$prim56321 = alloca %struct.ScmObj*, align 8
%_95k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55106)
store volatile %struct.ScmObj* %_95k48370, %struct.ScmObj** %stackaddr$prim56321, align 8
%stackaddr$prim56322 = alloca %struct.ScmObj*, align 8
%current_45args55107 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55106)
store volatile %struct.ScmObj* %current_45args55107, %struct.ScmObj** %stackaddr$prim56322, align 8
%stackaddr$prim56323 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55107)
store volatile %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$prim56323, align 8
%stackaddr$prim56324 = alloca %struct.ScmObj*, align 8
%anf_45bind48276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48146)
store volatile %struct.ScmObj* %anf_45bind48276, %struct.ScmObj** %stackaddr$prim56324, align 8
%stackaddr$prim56325 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48276)
store volatile %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$prim56325, align 8
%stackaddr$prim56326 = alloca %struct.ScmObj*, align 8
%cpsprim48371 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind48275, %struct.ScmObj* %anf_45bind48277)
store volatile %struct.ScmObj* %cpsprim48371, %struct.ScmObj** %stackaddr$prim56326, align 8
%ae51889 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55109$k483690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56327 = alloca %struct.ScmObj*, align 8
%argslist55109$k483691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48371, %struct.ScmObj* %argslist55109$k483690)
store volatile %struct.ScmObj* %argslist55109$k483691, %struct.ScmObj** %stackaddr$prim56327, align 8
%stackaddr$prim56328 = alloca %struct.ScmObj*, align 8
%argslist55109$k483692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51889, %struct.ScmObj* %argslist55109$k483691)
store volatile %struct.ScmObj* %argslist55109$k483692, %struct.ScmObj** %stackaddr$prim56328, align 8
%clofunc56329 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48369)
musttail call tailcc void %clofunc56329(%struct.ScmObj* %k48369, %struct.ScmObj* %argslist55109$k483692)
ret void
}

define tailcc void @proc_clo$ae51853(%struct.ScmObj* %env$ae51853,%struct.ScmObj* %current_45args55111) {
%stackaddr$prim56330 = alloca %struct.ScmObj*, align 8
%k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55111)
store volatile %struct.ScmObj* %k48373, %struct.ScmObj** %stackaddr$prim56330, align 8
%stackaddr$prim56331 = alloca %struct.ScmObj*, align 8
%current_45args55112 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55111)
store volatile %struct.ScmObj* %current_45args55112, %struct.ScmObj** %stackaddr$prim56331, align 8
%stackaddr$prim56332 = alloca %struct.ScmObj*, align 8
%x48084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55112)
store volatile %struct.ScmObj* %x48084, %struct.ScmObj** %stackaddr$prim56332, align 8
%stackaddr$prim56333 = alloca %struct.ScmObj*, align 8
%anf_45bind48272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48084)
store volatile %struct.ScmObj* %anf_45bind48272, %struct.ScmObj** %stackaddr$prim56333, align 8
%stackaddr$prim56334 = alloca %struct.ScmObj*, align 8
%anf_45bind48273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48272)
store volatile %struct.ScmObj* %anf_45bind48273, %struct.ScmObj** %stackaddr$prim56334, align 8
%stackaddr$prim56335 = alloca %struct.ScmObj*, align 8
%anf_45bind48274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48273)
store volatile %struct.ScmObj* %anf_45bind48274, %struct.ScmObj** %stackaddr$prim56335, align 8
%stackaddr$prim56336 = alloca %struct.ScmObj*, align 8
%cpsprim48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48274)
store volatile %struct.ScmObj* %cpsprim48374, %struct.ScmObj** %stackaddr$prim56336, align 8
%ae51859 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55114$k483730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56337 = alloca %struct.ScmObj*, align 8
%argslist55114$k483731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48374, %struct.ScmObj* %argslist55114$k483730)
store volatile %struct.ScmObj* %argslist55114$k483731, %struct.ScmObj** %stackaddr$prim56337, align 8
%stackaddr$prim56338 = alloca %struct.ScmObj*, align 8
%argslist55114$k483732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51859, %struct.ScmObj* %argslist55114$k483731)
store volatile %struct.ScmObj* %argslist55114$k483732, %struct.ScmObj** %stackaddr$prim56338, align 8
%clofunc56339 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48373)
musttail call tailcc void %clofunc56339(%struct.ScmObj* %k48373, %struct.ScmObj* %argslist55114$k483732)
ret void
}

define tailcc void @proc_clo$ae51829(%struct.ScmObj* %env$ae51829,%struct.ScmObj* %current_45args55116) {
%stackaddr$prim56340 = alloca %struct.ScmObj*, align 8
%k48375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55116)
store volatile %struct.ScmObj* %k48375, %struct.ScmObj** %stackaddr$prim56340, align 8
%stackaddr$prim56341 = alloca %struct.ScmObj*, align 8
%current_45args55117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55116)
store volatile %struct.ScmObj* %current_45args55117, %struct.ScmObj** %stackaddr$prim56341, align 8
%stackaddr$prim56342 = alloca %struct.ScmObj*, align 8
%x48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55117)
store volatile %struct.ScmObj* %x48086, %struct.ScmObj** %stackaddr$prim56342, align 8
%stackaddr$prim56343 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48086)
store volatile %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$prim56343, align 8
%stackaddr$prim56344 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48270)
store volatile %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$prim56344, align 8
%stackaddr$prim56345 = alloca %struct.ScmObj*, align 8
%cpsprim48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48271)
store volatile %struct.ScmObj* %cpsprim48376, %struct.ScmObj** %stackaddr$prim56345, align 8
%ae51834 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55119$k483750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56346 = alloca %struct.ScmObj*, align 8
%argslist55119$k483751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48376, %struct.ScmObj* %argslist55119$k483750)
store volatile %struct.ScmObj* %argslist55119$k483751, %struct.ScmObj** %stackaddr$prim56346, align 8
%stackaddr$prim56347 = alloca %struct.ScmObj*, align 8
%argslist55119$k483752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51834, %struct.ScmObj* %argslist55119$k483751)
store volatile %struct.ScmObj* %argslist55119$k483752, %struct.ScmObj** %stackaddr$prim56347, align 8
%clofunc56348 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48375)
musttail call tailcc void %clofunc56348(%struct.ScmObj* %k48375, %struct.ScmObj* %argslist55119$k483752)
ret void
}

define tailcc void @proc_clo$ae51807(%struct.ScmObj* %env$ae51807,%struct.ScmObj* %current_45args55121) {
%stackaddr$prim56349 = alloca %struct.ScmObj*, align 8
%k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55121)
store volatile %struct.ScmObj* %k48377, %struct.ScmObj** %stackaddr$prim56349, align 8
%stackaddr$prim56350 = alloca %struct.ScmObj*, align 8
%current_45args55122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55121)
store volatile %struct.ScmObj* %current_45args55122, %struct.ScmObj** %stackaddr$prim56350, align 8
%stackaddr$prim56351 = alloca %struct.ScmObj*, align 8
%x48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55122)
store volatile %struct.ScmObj* %x48088, %struct.ScmObj** %stackaddr$prim56351, align 8
%stackaddr$prim56352 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48088)
store volatile %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$prim56352, align 8
%stackaddr$prim56353 = alloca %struct.ScmObj*, align 8
%cpsprim48378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48269)
store volatile %struct.ScmObj* %cpsprim48378, %struct.ScmObj** %stackaddr$prim56353, align 8
%ae51811 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55124$k483770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56354 = alloca %struct.ScmObj*, align 8
%argslist55124$k483771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48378, %struct.ScmObj* %argslist55124$k483770)
store volatile %struct.ScmObj* %argslist55124$k483771, %struct.ScmObj** %stackaddr$prim56354, align 8
%stackaddr$prim56355 = alloca %struct.ScmObj*, align 8
%argslist55124$k483772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51811, %struct.ScmObj* %argslist55124$k483771)
store volatile %struct.ScmObj* %argslist55124$k483772, %struct.ScmObj** %stackaddr$prim56355, align 8
%clofunc56356 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48377)
musttail call tailcc void %clofunc56356(%struct.ScmObj* %k48377, %struct.ScmObj* %argslist55124$k483772)
ret void
}

define tailcc void @proc_clo$ae51787(%struct.ScmObj* %env$ae51787,%struct.ScmObj* %current_45args55126) {
%stackaddr$prim56357 = alloca %struct.ScmObj*, align 8
%k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55126)
store volatile %struct.ScmObj* %k48379, %struct.ScmObj** %stackaddr$prim56357, align 8
%stackaddr$prim56358 = alloca %struct.ScmObj*, align 8
%current_45args55127 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55126)
store volatile %struct.ScmObj* %current_45args55127, %struct.ScmObj** %stackaddr$prim56358, align 8
%stackaddr$prim56359 = alloca %struct.ScmObj*, align 8
%x48090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55127)
store volatile %struct.ScmObj* %x48090, %struct.ScmObj** %stackaddr$prim56359, align 8
%stackaddr$prim56360 = alloca %struct.ScmObj*, align 8
%cpsprim48380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48090)
store volatile %struct.ScmObj* %cpsprim48380, %struct.ScmObj** %stackaddr$prim56360, align 8
%ae51790 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55129$k483790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56361 = alloca %struct.ScmObj*, align 8
%argslist55129$k483791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48380, %struct.ScmObj* %argslist55129$k483790)
store volatile %struct.ScmObj* %argslist55129$k483791, %struct.ScmObj** %stackaddr$prim56361, align 8
%stackaddr$prim56362 = alloca %struct.ScmObj*, align 8
%argslist55129$k483792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51790, %struct.ScmObj* %argslist55129$k483791)
store volatile %struct.ScmObj* %argslist55129$k483792, %struct.ScmObj** %stackaddr$prim56362, align 8
%clofunc56363 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48379)
musttail call tailcc void %clofunc56363(%struct.ScmObj* %k48379, %struct.ScmObj* %argslist55129$k483792)
ret void
}

define tailcc void @proc_clo$ae51689(%struct.ScmObj* %env$ae51689,%struct.ScmObj* %args4809248381) {
%stackaddr$env-ref56364 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51689, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56364
%stackaddr$prim56365 = alloca %struct.ScmObj*, align 8
%k48382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809248381)
store volatile %struct.ScmObj* %k48382, %struct.ScmObj** %stackaddr$prim56365, align 8
%stackaddr$prim56366 = alloca %struct.ScmObj*, align 8
%args48092 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809248381)
store volatile %struct.ScmObj* %args48092, %struct.ScmObj** %stackaddr$prim56366, align 8
%stackaddr$prim56367 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim56367, align 8
%truthy$cmp56368 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48263)
%cmp$cmp56368 = icmp eq i64 %truthy$cmp56368, 1
br i1 %cmp$cmp56368, label %truebranch$cmp56368, label %falsebranch$cmp56368
truebranch$cmp56368:
%ae51695 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51696 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist55131$k483820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56369 = alloca %struct.ScmObj*, align 8
%argslist55131$k483821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51696, %struct.ScmObj* %argslist55131$k483820)
store volatile %struct.ScmObj* %argslist55131$k483821, %struct.ScmObj** %stackaddr$prim56369, align 8
%stackaddr$prim56370 = alloca %struct.ScmObj*, align 8
%argslist55131$k483822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51695, %struct.ScmObj* %argslist55131$k483821)
store volatile %struct.ScmObj* %argslist55131$k483822, %struct.ScmObj** %stackaddr$prim56370, align 8
%clofunc56371 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48382)
musttail call tailcc void %clofunc56371(%struct.ScmObj* %k48382, %struct.ScmObj* %argslist55131$k483822)
ret void
falsebranch$cmp56368:
%stackaddr$prim56372 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim56372, align 8
%stackaddr$prim56373 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48264)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim56373, align 8
%truthy$cmp56374 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48265)
%cmp$cmp56374 = icmp eq i64 %truthy$cmp56374, 1
br i1 %cmp$cmp56374, label %truebranch$cmp56374, label %falsebranch$cmp56374
truebranch$cmp56374:
%stackaddr$prim56375 = alloca %struct.ScmObj*, align 8
%cpsprim48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %cpsprim48383, %struct.ScmObj** %stackaddr$prim56375, align 8
%ae51708 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55132$k483820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56376 = alloca %struct.ScmObj*, align 8
%argslist55132$k483821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48383, %struct.ScmObj* %argslist55132$k483820)
store volatile %struct.ScmObj* %argslist55132$k483821, %struct.ScmObj** %stackaddr$prim56376, align 8
%stackaddr$prim56377 = alloca %struct.ScmObj*, align 8
%argslist55132$k483822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51708, %struct.ScmObj* %argslist55132$k483821)
store volatile %struct.ScmObj* %argslist55132$k483822, %struct.ScmObj** %stackaddr$prim56377, align 8
%clofunc56378 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48382)
musttail call tailcc void %clofunc56378(%struct.ScmObj* %k48382, %struct.ScmObj* %argslist55132$k483822)
ret void
falsebranch$cmp56374:
%stackaddr$makeclosure56379 = alloca %struct.ScmObj*, align 8
%fptrToInt56380 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51713 to i64
%ae51713 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56380)
store volatile %struct.ScmObj* %ae51713, %struct.ScmObj** %stackaddr$makeclosure56379, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51713, %struct.ScmObj* %_37foldl148031, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51713, %struct.ScmObj* %k48382, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51713, %struct.ScmObj* %args48092, i64 2)
%ae51714 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56381 = alloca %struct.ScmObj*, align 8
%fptrToInt56382 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51715 to i64
%ae51715 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56382)
store volatile %struct.ScmObj* %ae51715, %struct.ScmObj** %stackaddr$makeclosure56381, align 8
%argslist55142$ae517130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56383 = alloca %struct.ScmObj*, align 8
%argslist55142$ae517131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51715, %struct.ScmObj* %argslist55142$ae517130)
store volatile %struct.ScmObj* %argslist55142$ae517131, %struct.ScmObj** %stackaddr$prim56383, align 8
%stackaddr$prim56384 = alloca %struct.ScmObj*, align 8
%argslist55142$ae517132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51714, %struct.ScmObj* %argslist55142$ae517131)
store volatile %struct.ScmObj* %argslist55142$ae517132, %struct.ScmObj** %stackaddr$prim56384, align 8
%clofunc56385 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51713)
musttail call tailcc void %clofunc56385(%struct.ScmObj* %ae51713, %struct.ScmObj* %argslist55142$ae517132)
ret void
}

define tailcc void @proc_clo$ae51713(%struct.ScmObj* %env$ae51713,%struct.ScmObj* %current_45args55133) {
%stackaddr$env-ref56386 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51713, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56386
%stackaddr$env-ref56387 = alloca %struct.ScmObj*, align 8
%k48382 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51713, i64 1)
store %struct.ScmObj* %k48382, %struct.ScmObj** %stackaddr$env-ref56387
%stackaddr$env-ref56388 = alloca %struct.ScmObj*, align 8
%args48092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51713, i64 2)
store %struct.ScmObj* %args48092, %struct.ScmObj** %stackaddr$env-ref56388
%stackaddr$prim56389 = alloca %struct.ScmObj*, align 8
%_95k48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55133)
store volatile %struct.ScmObj* %_95k48384, %struct.ScmObj** %stackaddr$prim56389, align 8
%stackaddr$prim56390 = alloca %struct.ScmObj*, align 8
%current_45args55134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55133)
store volatile %struct.ScmObj* %current_45args55134, %struct.ScmObj** %stackaddr$prim56390, align 8
%stackaddr$prim56391 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55134)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim56391, align 8
%stackaddr$prim56392 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim56392, align 8
%stackaddr$prim56393 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$prim56393, align 8
%argslist55136$_37foldl1480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56394 = alloca %struct.ScmObj*, align 8
%argslist55136$_37foldl1480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48268, %struct.ScmObj* %argslist55136$_37foldl1480310)
store volatile %struct.ScmObj* %argslist55136$_37foldl1480311, %struct.ScmObj** %stackaddr$prim56394, align 8
%stackaddr$prim56395 = alloca %struct.ScmObj*, align 8
%argslist55136$_37foldl1480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48267, %struct.ScmObj* %argslist55136$_37foldl1480311)
store volatile %struct.ScmObj* %argslist55136$_37foldl1480312, %struct.ScmObj** %stackaddr$prim56395, align 8
%stackaddr$prim56396 = alloca %struct.ScmObj*, align 8
%argslist55136$_37foldl1480313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48266, %struct.ScmObj* %argslist55136$_37foldl1480312)
store volatile %struct.ScmObj* %argslist55136$_37foldl1480313, %struct.ScmObj** %stackaddr$prim56396, align 8
%stackaddr$prim56397 = alloca %struct.ScmObj*, align 8
%argslist55136$_37foldl1480314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48382, %struct.ScmObj* %argslist55136$_37foldl1480313)
store volatile %struct.ScmObj* %argslist55136$_37foldl1480314, %struct.ScmObj** %stackaddr$prim56397, align 8
%clofunc56398 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148031)
musttail call tailcc void %clofunc56398(%struct.ScmObj* %_37foldl148031, %struct.ScmObj* %argslist55136$_37foldl1480314)
ret void
}

define tailcc void @proc_clo$ae51715(%struct.ScmObj* %env$ae51715,%struct.ScmObj* %current_45args55137) {
%stackaddr$prim56399 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55137)
store volatile %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$prim56399, align 8
%stackaddr$prim56400 = alloca %struct.ScmObj*, align 8
%current_45args55138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55137)
store volatile %struct.ScmObj* %current_45args55138, %struct.ScmObj** %stackaddr$prim56400, align 8
%stackaddr$prim56401 = alloca %struct.ScmObj*, align 8
%n48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55138)
store volatile %struct.ScmObj* %n48094, %struct.ScmObj** %stackaddr$prim56401, align 8
%stackaddr$prim56402 = alloca %struct.ScmObj*, align 8
%current_45args55139 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55138)
store volatile %struct.ScmObj* %current_45args55139, %struct.ScmObj** %stackaddr$prim56402, align 8
%stackaddr$prim56403 = alloca %struct.ScmObj*, align 8
%v48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55139)
store volatile %struct.ScmObj* %v48093, %struct.ScmObj** %stackaddr$prim56403, align 8
%stackaddr$prim56404 = alloca %struct.ScmObj*, align 8
%cpsprim48386 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48093, %struct.ScmObj* %n48094)
store volatile %struct.ScmObj* %cpsprim48386, %struct.ScmObj** %stackaddr$prim56404, align 8
%ae51719 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55141$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56405 = alloca %struct.ScmObj*, align 8
%argslist55141$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48386, %struct.ScmObj* %argslist55141$k483850)
store volatile %struct.ScmObj* %argslist55141$k483851, %struct.ScmObj** %stackaddr$prim56405, align 8
%stackaddr$prim56406 = alloca %struct.ScmObj*, align 8
%argslist55141$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51719, %struct.ScmObj* %argslist55141$k483851)
store volatile %struct.ScmObj* %argslist55141$k483852, %struct.ScmObj** %stackaddr$prim56406, align 8
%clofunc56407 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc56407(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist55141$k483852)
ret void
}

define tailcc void @proc_clo$ae51285(%struct.ScmObj* %env$ae51285,%struct.ScmObj* %current_45args55144) {
%stackaddr$prim56408 = alloca %struct.ScmObj*, align 8
%k48387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55144)
store volatile %struct.ScmObj* %k48387, %struct.ScmObj** %stackaddr$prim56408, align 8
%stackaddr$prim56409 = alloca %struct.ScmObj*, align 8
%current_45args55145 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55144)
store volatile %struct.ScmObj* %current_45args55145, %struct.ScmObj** %stackaddr$prim56409, align 8
%stackaddr$prim56410 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55145)
store volatile %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$prim56410, align 8
%stackaddr$prim56411 = alloca %struct.ScmObj*, align 8
%current_45args55146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55145)
store volatile %struct.ScmObj* %current_45args55146, %struct.ScmObj** %stackaddr$prim56411, align 8
%stackaddr$prim56412 = alloca %struct.ScmObj*, align 8
%lst48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55146)
store volatile %struct.ScmObj* %lst48096, %struct.ScmObj** %stackaddr$prim56412, align 8
%ae51286 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56413 = alloca %struct.ScmObj*, align 8
%lst48098 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51286, %struct.ScmObj* %lst48096)
store volatile %struct.ScmObj* %lst48098, %struct.ScmObj** %stackaddr$prim56413, align 8
%stackaddr$makeclosure56414 = alloca %struct.ScmObj*, align 8
%fptrToInt56415 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51288 to i64
%ae51288 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56415)
store volatile %struct.ScmObj* %ae51288, %struct.ScmObj** %stackaddr$makeclosure56414, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51288, %struct.ScmObj* %k48387, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51288, %struct.ScmObj* %lst48098, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51288, %struct.ScmObj* %v48097, i64 2)
%ae51289 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56416 = alloca %struct.ScmObj*, align 8
%fptrToInt56417 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51290 to i64
%ae51290 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56417)
store volatile %struct.ScmObj* %ae51290, %struct.ScmObj** %stackaddr$makeclosure56416, align 8
%argslist55168$ae512880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56418 = alloca %struct.ScmObj*, align 8
%argslist55168$ae512881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51290, %struct.ScmObj* %argslist55168$ae512880)
store volatile %struct.ScmObj* %argslist55168$ae512881, %struct.ScmObj** %stackaddr$prim56418, align 8
%stackaddr$prim56419 = alloca %struct.ScmObj*, align 8
%argslist55168$ae512882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51289, %struct.ScmObj* %argslist55168$ae512881)
store volatile %struct.ScmObj* %argslist55168$ae512882, %struct.ScmObj** %stackaddr$prim56419, align 8
%clofunc56420 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51288)
musttail call tailcc void %clofunc56420(%struct.ScmObj* %ae51288, %struct.ScmObj* %argslist55168$ae512882)
ret void
}

define tailcc void @proc_clo$ae51288(%struct.ScmObj* %env$ae51288,%struct.ScmObj* %current_45args55148) {
%stackaddr$env-ref56421 = alloca %struct.ScmObj*, align 8
%k48387 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51288, i64 0)
store %struct.ScmObj* %k48387, %struct.ScmObj** %stackaddr$env-ref56421
%stackaddr$env-ref56422 = alloca %struct.ScmObj*, align 8
%lst48098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51288, i64 1)
store %struct.ScmObj* %lst48098, %struct.ScmObj** %stackaddr$env-ref56422
%stackaddr$env-ref56423 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51288, i64 2)
store %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$env-ref56423
%stackaddr$prim56424 = alloca %struct.ScmObj*, align 8
%_95k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55148)
store volatile %struct.ScmObj* %_95k48388, %struct.ScmObj** %stackaddr$prim56424, align 8
%stackaddr$prim56425 = alloca %struct.ScmObj*, align 8
%current_45args55149 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55148)
store volatile %struct.ScmObj* %current_45args55149, %struct.ScmObj** %stackaddr$prim56425, align 8
%stackaddr$prim56426 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55149)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim56426, align 8
%stackaddr$makeclosure56427 = alloca %struct.ScmObj*, align 8
%fptrToInt56428 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51304 to i64
%ae51304 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56428)
store volatile %struct.ScmObj* %ae51304, %struct.ScmObj** %stackaddr$makeclosure56427, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51304, %struct.ScmObj* %k48387, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51304, %struct.ScmObj* %lst48098, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51304, %struct.ScmObj* %v48097, i64 2)
%stackaddr$makeclosure56429 = alloca %struct.ScmObj*, align 8
%fptrToInt56430 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51305 to i64
%ae51305 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56430)
store volatile %struct.ScmObj* %ae51305, %struct.ScmObj** %stackaddr$makeclosure56429, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51305, %struct.ScmObj* %k48387, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51305, %struct.ScmObj* %lst48098, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51305, %struct.ScmObj* %v48097, i64 2)
%argslist55163$anf_45bind482550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56431 = alloca %struct.ScmObj*, align 8
%argslist55163$anf_45bind482551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51305, %struct.ScmObj* %argslist55163$anf_45bind482550)
store volatile %struct.ScmObj* %argslist55163$anf_45bind482551, %struct.ScmObj** %stackaddr$prim56431, align 8
%stackaddr$prim56432 = alloca %struct.ScmObj*, align 8
%argslist55163$anf_45bind482552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51304, %struct.ScmObj* %argslist55163$anf_45bind482551)
store volatile %struct.ScmObj* %argslist55163$anf_45bind482552, %struct.ScmObj** %stackaddr$prim56432, align 8
%clofunc56433 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48255)
musttail call tailcc void %clofunc56433(%struct.ScmObj* %anf_45bind48255, %struct.ScmObj* %argslist55163$anf_45bind482552)
ret void
}

define tailcc void @proc_clo$ae51304(%struct.ScmObj* %env$ae51304,%struct.ScmObj* %current_45args55151) {
%stackaddr$env-ref56434 = alloca %struct.ScmObj*, align 8
%k48387 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51304, i64 0)
store %struct.ScmObj* %k48387, %struct.ScmObj** %stackaddr$env-ref56434
%stackaddr$env-ref56435 = alloca %struct.ScmObj*, align 8
%lst48098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51304, i64 1)
store %struct.ScmObj* %lst48098, %struct.ScmObj** %stackaddr$env-ref56435
%stackaddr$env-ref56436 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51304, i64 2)
store %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$env-ref56436
%stackaddr$prim56437 = alloca %struct.ScmObj*, align 8
%_95k48389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55151)
store volatile %struct.ScmObj* %_95k48389, %struct.ScmObj** %stackaddr$prim56437, align 8
%stackaddr$prim56438 = alloca %struct.ScmObj*, align 8
%current_45args55152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55151)
store volatile %struct.ScmObj* %current_45args55152, %struct.ScmObj** %stackaddr$prim56438, align 8
%stackaddr$prim56439 = alloca %struct.ScmObj*, align 8
%cc48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55152)
store volatile %struct.ScmObj* %cc48099, %struct.ScmObj** %stackaddr$prim56439, align 8
%ae51413 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56440 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51413)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim56440, align 8
%stackaddr$prim56441 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48256)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim56441, align 8
%truthy$cmp56442 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48257)
%cmp$cmp56442 = icmp eq i64 %truthy$cmp56442, 1
br i1 %cmp$cmp56442, label %truebranch$cmp56442, label %falsebranch$cmp56442
truebranch$cmp56442:
%ae51417 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51418 = call %struct.ScmObj* @const_init_false()
%argslist55154$k483870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56443 = alloca %struct.ScmObj*, align 8
%argslist55154$k483871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51418, %struct.ScmObj* %argslist55154$k483870)
store volatile %struct.ScmObj* %argslist55154$k483871, %struct.ScmObj** %stackaddr$prim56443, align 8
%stackaddr$prim56444 = alloca %struct.ScmObj*, align 8
%argslist55154$k483872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51417, %struct.ScmObj* %argslist55154$k483871)
store volatile %struct.ScmObj* %argslist55154$k483872, %struct.ScmObj** %stackaddr$prim56444, align 8
%clofunc56445 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48387)
musttail call tailcc void %clofunc56445(%struct.ScmObj* %k48387, %struct.ScmObj* %argslist55154$k483872)
ret void
falsebranch$cmp56442:
%ae51426 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56446 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51426)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim56446, align 8
%stackaddr$prim56447 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48258)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim56447, align 8
%stackaddr$prim56448 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48259, %struct.ScmObj* %v48097)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim56448, align 8
%truthy$cmp56449 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48260)
%cmp$cmp56449 = icmp eq i64 %truthy$cmp56449, 1
br i1 %cmp$cmp56449, label %truebranch$cmp56449, label %falsebranch$cmp56449
truebranch$cmp56449:
%ae51432 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56450 = alloca %struct.ScmObj*, align 8
%cpsprim48390 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51432)
store volatile %struct.ScmObj* %cpsprim48390, %struct.ScmObj** %stackaddr$prim56450, align 8
%ae51434 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55155$k483870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56451 = alloca %struct.ScmObj*, align 8
%argslist55155$k483871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48390, %struct.ScmObj* %argslist55155$k483870)
store volatile %struct.ScmObj* %argslist55155$k483871, %struct.ScmObj** %stackaddr$prim56451, align 8
%stackaddr$prim56452 = alloca %struct.ScmObj*, align 8
%argslist55155$k483872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51434, %struct.ScmObj* %argslist55155$k483871)
store volatile %struct.ScmObj* %argslist55155$k483872, %struct.ScmObj** %stackaddr$prim56452, align 8
%clofunc56453 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48387)
musttail call tailcc void %clofunc56453(%struct.ScmObj* %k48387, %struct.ScmObj* %argslist55155$k483872)
ret void
falsebranch$cmp56449:
%ae51445 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56454 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51445)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim56454, align 8
%stackaddr$prim56455 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48261)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim56455, align 8
%ae51448 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56456 = alloca %struct.ScmObj*, align 8
%_95048101 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51448, %struct.ScmObj* %anf_45bind48262)
store volatile %struct.ScmObj* %_95048101, %struct.ScmObj** %stackaddr$prim56456, align 8
%argslist55156$cc480990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56457 = alloca %struct.ScmObj*, align 8
%argslist55156$cc480991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48099, %struct.ScmObj* %argslist55156$cc480990)
store volatile %struct.ScmObj* %argslist55156$cc480991, %struct.ScmObj** %stackaddr$prim56457, align 8
%stackaddr$prim56458 = alloca %struct.ScmObj*, align 8
%argslist55156$cc480992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48387, %struct.ScmObj* %argslist55156$cc480991)
store volatile %struct.ScmObj* %argslist55156$cc480992, %struct.ScmObj** %stackaddr$prim56458, align 8
%clofunc56459 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48099)
musttail call tailcc void %clofunc56459(%struct.ScmObj* %cc48099, %struct.ScmObj* %argslist55156$cc480992)
ret void
}

define tailcc void @proc_clo$ae51305(%struct.ScmObj* %env$ae51305,%struct.ScmObj* %current_45args55157) {
%stackaddr$env-ref56460 = alloca %struct.ScmObj*, align 8
%k48387 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51305, i64 0)
store %struct.ScmObj* %k48387, %struct.ScmObj** %stackaddr$env-ref56460
%stackaddr$env-ref56461 = alloca %struct.ScmObj*, align 8
%lst48098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51305, i64 1)
store %struct.ScmObj* %lst48098, %struct.ScmObj** %stackaddr$env-ref56461
%stackaddr$env-ref56462 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51305, i64 2)
store %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$env-ref56462
%stackaddr$prim56463 = alloca %struct.ScmObj*, align 8
%_95k48389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55157)
store volatile %struct.ScmObj* %_95k48389, %struct.ScmObj** %stackaddr$prim56463, align 8
%stackaddr$prim56464 = alloca %struct.ScmObj*, align 8
%current_45args55158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55157)
store volatile %struct.ScmObj* %current_45args55158, %struct.ScmObj** %stackaddr$prim56464, align 8
%stackaddr$prim56465 = alloca %struct.ScmObj*, align 8
%cc48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55158)
store volatile %struct.ScmObj* %cc48099, %struct.ScmObj** %stackaddr$prim56465, align 8
%ae51307 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56466 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51307)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim56466, align 8
%stackaddr$prim56467 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48256)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim56467, align 8
%truthy$cmp56468 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48257)
%cmp$cmp56468 = icmp eq i64 %truthy$cmp56468, 1
br i1 %cmp$cmp56468, label %truebranch$cmp56468, label %falsebranch$cmp56468
truebranch$cmp56468:
%ae51311 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51312 = call %struct.ScmObj* @const_init_false()
%argslist55160$k483870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56469 = alloca %struct.ScmObj*, align 8
%argslist55160$k483871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51312, %struct.ScmObj* %argslist55160$k483870)
store volatile %struct.ScmObj* %argslist55160$k483871, %struct.ScmObj** %stackaddr$prim56469, align 8
%stackaddr$prim56470 = alloca %struct.ScmObj*, align 8
%argslist55160$k483872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51311, %struct.ScmObj* %argslist55160$k483871)
store volatile %struct.ScmObj* %argslist55160$k483872, %struct.ScmObj** %stackaddr$prim56470, align 8
%clofunc56471 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48387)
musttail call tailcc void %clofunc56471(%struct.ScmObj* %k48387, %struct.ScmObj* %argslist55160$k483872)
ret void
falsebranch$cmp56468:
%ae51320 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56472 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51320)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim56472, align 8
%stackaddr$prim56473 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48258)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim56473, align 8
%stackaddr$prim56474 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48259, %struct.ScmObj* %v48097)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim56474, align 8
%truthy$cmp56475 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48260)
%cmp$cmp56475 = icmp eq i64 %truthy$cmp56475, 1
br i1 %cmp$cmp56475, label %truebranch$cmp56475, label %falsebranch$cmp56475
truebranch$cmp56475:
%ae51326 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56476 = alloca %struct.ScmObj*, align 8
%cpsprim48390 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51326)
store volatile %struct.ScmObj* %cpsprim48390, %struct.ScmObj** %stackaddr$prim56476, align 8
%ae51328 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55161$k483870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56477 = alloca %struct.ScmObj*, align 8
%argslist55161$k483871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48390, %struct.ScmObj* %argslist55161$k483870)
store volatile %struct.ScmObj* %argslist55161$k483871, %struct.ScmObj** %stackaddr$prim56477, align 8
%stackaddr$prim56478 = alloca %struct.ScmObj*, align 8
%argslist55161$k483872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51328, %struct.ScmObj* %argslist55161$k483871)
store volatile %struct.ScmObj* %argslist55161$k483872, %struct.ScmObj** %stackaddr$prim56478, align 8
%clofunc56479 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48387)
musttail call tailcc void %clofunc56479(%struct.ScmObj* %k48387, %struct.ScmObj* %argslist55161$k483872)
ret void
falsebranch$cmp56475:
%ae51339 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56480 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51339)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim56480, align 8
%stackaddr$prim56481 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48261)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim56481, align 8
%ae51342 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56482 = alloca %struct.ScmObj*, align 8
%_95048101 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51342, %struct.ScmObj* %anf_45bind48262)
store volatile %struct.ScmObj* %_95048101, %struct.ScmObj** %stackaddr$prim56482, align 8
%argslist55162$cc480990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56483 = alloca %struct.ScmObj*, align 8
%argslist55162$cc480991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48099, %struct.ScmObj* %argslist55162$cc480990)
store volatile %struct.ScmObj* %argslist55162$cc480991, %struct.ScmObj** %stackaddr$prim56483, align 8
%stackaddr$prim56484 = alloca %struct.ScmObj*, align 8
%argslist55162$cc480992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48387, %struct.ScmObj* %argslist55162$cc480991)
store volatile %struct.ScmObj* %argslist55162$cc480992, %struct.ScmObj** %stackaddr$prim56484, align 8
%clofunc56485 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48099)
musttail call tailcc void %clofunc56485(%struct.ScmObj* %cc48099, %struct.ScmObj* %argslist55162$cc480992)
ret void
}

define tailcc void @proc_clo$ae51290(%struct.ScmObj* %env$ae51290,%struct.ScmObj* %current_45args55164) {
%stackaddr$prim56486 = alloca %struct.ScmObj*, align 8
%k48391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55164)
store volatile %struct.ScmObj* %k48391, %struct.ScmObj** %stackaddr$prim56486, align 8
%stackaddr$prim56487 = alloca %struct.ScmObj*, align 8
%current_45args55165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55164)
store volatile %struct.ScmObj* %current_45args55165, %struct.ScmObj** %stackaddr$prim56487, align 8
%stackaddr$prim56488 = alloca %struct.ScmObj*, align 8
%u48100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55165)
store volatile %struct.ScmObj* %u48100, %struct.ScmObj** %stackaddr$prim56488, align 8
%argslist55167$u481000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56489 = alloca %struct.ScmObj*, align 8
%argslist55167$u481001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48100, %struct.ScmObj* %argslist55167$u481000)
store volatile %struct.ScmObj* %argslist55167$u481001, %struct.ScmObj** %stackaddr$prim56489, align 8
%stackaddr$prim56490 = alloca %struct.ScmObj*, align 8
%argslist55167$u481002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48391, %struct.ScmObj* %argslist55167$u481001)
store volatile %struct.ScmObj* %argslist55167$u481002, %struct.ScmObj** %stackaddr$prim56490, align 8
%clofunc56491 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48100)
musttail call tailcc void %clofunc56491(%struct.ScmObj* %u48100, %struct.ScmObj* %argslist55167$u481002)
ret void
}

define tailcc void @proc_clo$ae50749(%struct.ScmObj* %env$ae50749,%struct.ScmObj* %current_45args55170) {
%stackaddr$prim56492 = alloca %struct.ScmObj*, align 8
%k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55170)
store volatile %struct.ScmObj* %k48392, %struct.ScmObj** %stackaddr$prim56492, align 8
%stackaddr$prim56493 = alloca %struct.ScmObj*, align 8
%current_45args55171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55170)
store volatile %struct.ScmObj* %current_45args55171, %struct.ScmObj** %stackaddr$prim56493, align 8
%stackaddr$prim56494 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55171)
store volatile %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$prim56494, align 8
%stackaddr$prim56495 = alloca %struct.ScmObj*, align 8
%current_45args55172 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55171)
store volatile %struct.ScmObj* %current_45args55172, %struct.ScmObj** %stackaddr$prim56495, align 8
%stackaddr$prim56496 = alloca %struct.ScmObj*, align 8
%n48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55172)
store volatile %struct.ScmObj* %n48103, %struct.ScmObj** %stackaddr$prim56496, align 8
%ae50750 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56497 = alloca %struct.ScmObj*, align 8
%n48106 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50750, %struct.ScmObj* %n48103)
store volatile %struct.ScmObj* %n48106, %struct.ScmObj** %stackaddr$prim56497, align 8
%ae50752 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56498 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50752, %struct.ScmObj* %lst48104)
store volatile %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$prim56498, align 8
%stackaddr$makeclosure56499 = alloca %struct.ScmObj*, align 8
%fptrToInt56500 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50754 to i64
%ae50754 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56500)
store volatile %struct.ScmObj* %ae50754, %struct.ScmObj** %stackaddr$makeclosure56499, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50754, %struct.ScmObj* %lst48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50754, %struct.ScmObj* %k48392, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50754, %struct.ScmObj* %n48106, i64 2)
%ae50755 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56501 = alloca %struct.ScmObj*, align 8
%fptrToInt56502 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50756 to i64
%ae50756 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56502)
store volatile %struct.ScmObj* %ae50756, %struct.ScmObj** %stackaddr$makeclosure56501, align 8
%argslist55192$ae507540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56503 = alloca %struct.ScmObj*, align 8
%argslist55192$ae507541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50756, %struct.ScmObj* %argslist55192$ae507540)
store volatile %struct.ScmObj* %argslist55192$ae507541, %struct.ScmObj** %stackaddr$prim56503, align 8
%stackaddr$prim56504 = alloca %struct.ScmObj*, align 8
%argslist55192$ae507542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50755, %struct.ScmObj* %argslist55192$ae507541)
store volatile %struct.ScmObj* %argslist55192$ae507542, %struct.ScmObj** %stackaddr$prim56504, align 8
%clofunc56505 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50754)
musttail call tailcc void %clofunc56505(%struct.ScmObj* %ae50754, %struct.ScmObj* %argslist55192$ae507542)
ret void
}

define tailcc void @proc_clo$ae50754(%struct.ScmObj* %env$ae50754,%struct.ScmObj* %current_45args55174) {
%stackaddr$env-ref56506 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50754, i64 0)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref56506
%stackaddr$env-ref56507 = alloca %struct.ScmObj*, align 8
%k48392 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50754, i64 1)
store %struct.ScmObj* %k48392, %struct.ScmObj** %stackaddr$env-ref56507
%stackaddr$env-ref56508 = alloca %struct.ScmObj*, align 8
%n48106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50754, i64 2)
store %struct.ScmObj* %n48106, %struct.ScmObj** %stackaddr$env-ref56508
%stackaddr$prim56509 = alloca %struct.ScmObj*, align 8
%_95k48393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55174)
store volatile %struct.ScmObj* %_95k48393, %struct.ScmObj** %stackaddr$prim56509, align 8
%stackaddr$prim56510 = alloca %struct.ScmObj*, align 8
%current_45args55175 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55174)
store volatile %struct.ScmObj* %current_45args55175, %struct.ScmObj** %stackaddr$prim56510, align 8
%stackaddr$prim56511 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55175)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim56511, align 8
%stackaddr$makeclosure56512 = alloca %struct.ScmObj*, align 8
%fptrToInt56513 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50770 to i64
%ae50770 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56513)
store volatile %struct.ScmObj* %ae50770, %struct.ScmObj** %stackaddr$makeclosure56512, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50770, %struct.ScmObj* %lst48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50770, %struct.ScmObj* %k48392, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50770, %struct.ScmObj* %n48106, i64 2)
%stackaddr$makeclosure56514 = alloca %struct.ScmObj*, align 8
%fptrToInt56515 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50771 to i64
%ae50771 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56515)
store volatile %struct.ScmObj* %ae50771, %struct.ScmObj** %stackaddr$makeclosure56514, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50771, %struct.ScmObj* %lst48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50771, %struct.ScmObj* %k48392, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50771, %struct.ScmObj* %n48106, i64 2)
%argslist55187$anf_45bind482480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56516 = alloca %struct.ScmObj*, align 8
%argslist55187$anf_45bind482481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50771, %struct.ScmObj* %argslist55187$anf_45bind482480)
store volatile %struct.ScmObj* %argslist55187$anf_45bind482481, %struct.ScmObj** %stackaddr$prim56516, align 8
%stackaddr$prim56517 = alloca %struct.ScmObj*, align 8
%argslist55187$anf_45bind482482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50770, %struct.ScmObj* %argslist55187$anf_45bind482481)
store volatile %struct.ScmObj* %argslist55187$anf_45bind482482, %struct.ScmObj** %stackaddr$prim56517, align 8
%clofunc56518 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48248)
musttail call tailcc void %clofunc56518(%struct.ScmObj* %anf_45bind48248, %struct.ScmObj* %argslist55187$anf_45bind482482)
ret void
}

define tailcc void @proc_clo$ae50770(%struct.ScmObj* %env$ae50770,%struct.ScmObj* %current_45args55177) {
%stackaddr$env-ref56519 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50770, i64 0)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref56519
%stackaddr$env-ref56520 = alloca %struct.ScmObj*, align 8
%k48392 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50770, i64 1)
store %struct.ScmObj* %k48392, %struct.ScmObj** %stackaddr$env-ref56520
%stackaddr$env-ref56521 = alloca %struct.ScmObj*, align 8
%n48106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50770, i64 2)
store %struct.ScmObj* %n48106, %struct.ScmObj** %stackaddr$env-ref56521
%stackaddr$prim56522 = alloca %struct.ScmObj*, align 8
%_95k48394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55177)
store volatile %struct.ScmObj* %_95k48394, %struct.ScmObj** %stackaddr$prim56522, align 8
%stackaddr$prim56523 = alloca %struct.ScmObj*, align 8
%current_45args55178 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55177)
store volatile %struct.ScmObj* %current_45args55178, %struct.ScmObj** %stackaddr$prim56523, align 8
%stackaddr$prim56524 = alloca %struct.ScmObj*, align 8
%cc48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55178)
store volatile %struct.ScmObj* %cc48107, %struct.ScmObj** %stackaddr$prim56524, align 8
%ae50913 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56525 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50913)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim56525, align 8
%ae50914 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56526 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50914, %struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim56526, align 8
%truthy$cmp56527 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48250)
%cmp$cmp56527 = icmp eq i64 %truthy$cmp56527, 1
br i1 %cmp$cmp56527, label %truebranch$cmp56527, label %falsebranch$cmp56527
truebranch$cmp56527:
%ae50918 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56528 = alloca %struct.ScmObj*, align 8
%cpsprim48395 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50918)
store volatile %struct.ScmObj* %cpsprim48395, %struct.ScmObj** %stackaddr$prim56528, align 8
%ae50920 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55180$k483920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56529 = alloca %struct.ScmObj*, align 8
%argslist55180$k483921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48395, %struct.ScmObj* %argslist55180$k483920)
store volatile %struct.ScmObj* %argslist55180$k483921, %struct.ScmObj** %stackaddr$prim56529, align 8
%stackaddr$prim56530 = alloca %struct.ScmObj*, align 8
%argslist55180$k483922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50920, %struct.ScmObj* %argslist55180$k483921)
store volatile %struct.ScmObj* %argslist55180$k483922, %struct.ScmObj** %stackaddr$prim56530, align 8
%clofunc56531 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48392)
musttail call tailcc void %clofunc56531(%struct.ScmObj* %k48392, %struct.ScmObj* %argslist55180$k483922)
ret void
falsebranch$cmp56527:
%ae50931 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56532 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50931)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim56532, align 8
%stackaddr$prim56533 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim56533, align 8
%ae50934 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56534 = alloca %struct.ScmObj*, align 8
%_95048110 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50934, %struct.ScmObj* %anf_45bind48252)
store volatile %struct.ScmObj* %_95048110, %struct.ScmObj** %stackaddr$prim56534, align 8
%ae50937 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56535 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50937)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim56535, align 8
%ae50939 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56536 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48253, %struct.ScmObj* %ae50939)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim56536, align 8
%ae50941 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56537 = alloca %struct.ScmObj*, align 8
%_95148109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50941, %struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %_95148109, %struct.ScmObj** %stackaddr$prim56537, align 8
%argslist55181$cc481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56538 = alloca %struct.ScmObj*, align 8
%argslist55181$cc481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48107, %struct.ScmObj* %argslist55181$cc481070)
store volatile %struct.ScmObj* %argslist55181$cc481071, %struct.ScmObj** %stackaddr$prim56538, align 8
%stackaddr$prim56539 = alloca %struct.ScmObj*, align 8
%argslist55181$cc481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48392, %struct.ScmObj* %argslist55181$cc481071)
store volatile %struct.ScmObj* %argslist55181$cc481072, %struct.ScmObj** %stackaddr$prim56539, align 8
%clofunc56540 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48107)
musttail call tailcc void %clofunc56540(%struct.ScmObj* %cc48107, %struct.ScmObj* %argslist55181$cc481072)
ret void
}

define tailcc void @proc_clo$ae50771(%struct.ScmObj* %env$ae50771,%struct.ScmObj* %current_45args55182) {
%stackaddr$env-ref56541 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50771, i64 0)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref56541
%stackaddr$env-ref56542 = alloca %struct.ScmObj*, align 8
%k48392 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50771, i64 1)
store %struct.ScmObj* %k48392, %struct.ScmObj** %stackaddr$env-ref56542
%stackaddr$env-ref56543 = alloca %struct.ScmObj*, align 8
%n48106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50771, i64 2)
store %struct.ScmObj* %n48106, %struct.ScmObj** %stackaddr$env-ref56543
%stackaddr$prim56544 = alloca %struct.ScmObj*, align 8
%_95k48394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55182)
store volatile %struct.ScmObj* %_95k48394, %struct.ScmObj** %stackaddr$prim56544, align 8
%stackaddr$prim56545 = alloca %struct.ScmObj*, align 8
%current_45args55183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55182)
store volatile %struct.ScmObj* %current_45args55183, %struct.ScmObj** %stackaddr$prim56545, align 8
%stackaddr$prim56546 = alloca %struct.ScmObj*, align 8
%cc48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55183)
store volatile %struct.ScmObj* %cc48107, %struct.ScmObj** %stackaddr$prim56546, align 8
%ae50773 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56547 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50773)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim56547, align 8
%ae50774 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56548 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50774, %struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim56548, align 8
%truthy$cmp56549 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48250)
%cmp$cmp56549 = icmp eq i64 %truthy$cmp56549, 1
br i1 %cmp$cmp56549, label %truebranch$cmp56549, label %falsebranch$cmp56549
truebranch$cmp56549:
%ae50778 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56550 = alloca %struct.ScmObj*, align 8
%cpsprim48395 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50778)
store volatile %struct.ScmObj* %cpsprim48395, %struct.ScmObj** %stackaddr$prim56550, align 8
%ae50780 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55185$k483920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56551 = alloca %struct.ScmObj*, align 8
%argslist55185$k483921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48395, %struct.ScmObj* %argslist55185$k483920)
store volatile %struct.ScmObj* %argslist55185$k483921, %struct.ScmObj** %stackaddr$prim56551, align 8
%stackaddr$prim56552 = alloca %struct.ScmObj*, align 8
%argslist55185$k483922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50780, %struct.ScmObj* %argslist55185$k483921)
store volatile %struct.ScmObj* %argslist55185$k483922, %struct.ScmObj** %stackaddr$prim56552, align 8
%clofunc56553 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48392)
musttail call tailcc void %clofunc56553(%struct.ScmObj* %k48392, %struct.ScmObj* %argslist55185$k483922)
ret void
falsebranch$cmp56549:
%ae50791 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56554 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50791)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim56554, align 8
%stackaddr$prim56555 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim56555, align 8
%ae50794 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56556 = alloca %struct.ScmObj*, align 8
%_95048110 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50794, %struct.ScmObj* %anf_45bind48252)
store volatile %struct.ScmObj* %_95048110, %struct.ScmObj** %stackaddr$prim56556, align 8
%ae50797 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56557 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50797)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim56557, align 8
%ae50799 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56558 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48253, %struct.ScmObj* %ae50799)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim56558, align 8
%ae50801 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56559 = alloca %struct.ScmObj*, align 8
%_95148109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50801, %struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %_95148109, %struct.ScmObj** %stackaddr$prim56559, align 8
%argslist55186$cc481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56560 = alloca %struct.ScmObj*, align 8
%argslist55186$cc481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48107, %struct.ScmObj* %argslist55186$cc481070)
store volatile %struct.ScmObj* %argslist55186$cc481071, %struct.ScmObj** %stackaddr$prim56560, align 8
%stackaddr$prim56561 = alloca %struct.ScmObj*, align 8
%argslist55186$cc481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48392, %struct.ScmObj* %argslist55186$cc481071)
store volatile %struct.ScmObj* %argslist55186$cc481072, %struct.ScmObj** %stackaddr$prim56561, align 8
%clofunc56562 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48107)
musttail call tailcc void %clofunc56562(%struct.ScmObj* %cc48107, %struct.ScmObj* %argslist55186$cc481072)
ret void
}

define tailcc void @proc_clo$ae50756(%struct.ScmObj* %env$ae50756,%struct.ScmObj* %current_45args55188) {
%stackaddr$prim56563 = alloca %struct.ScmObj*, align 8
%k48396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55188)
store volatile %struct.ScmObj* %k48396, %struct.ScmObj** %stackaddr$prim56563, align 8
%stackaddr$prim56564 = alloca %struct.ScmObj*, align 8
%current_45args55189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55188)
store volatile %struct.ScmObj* %current_45args55189, %struct.ScmObj** %stackaddr$prim56564, align 8
%stackaddr$prim56565 = alloca %struct.ScmObj*, align 8
%u48108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55189)
store volatile %struct.ScmObj* %u48108, %struct.ScmObj** %stackaddr$prim56565, align 8
%argslist55191$u481080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56566 = alloca %struct.ScmObj*, align 8
%argslist55191$u481081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48108, %struct.ScmObj* %argslist55191$u481080)
store volatile %struct.ScmObj* %argslist55191$u481081, %struct.ScmObj** %stackaddr$prim56566, align 8
%stackaddr$prim56567 = alloca %struct.ScmObj*, align 8
%argslist55191$u481082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48396, %struct.ScmObj* %argslist55191$u481081)
store volatile %struct.ScmObj* %argslist55191$u481082, %struct.ScmObj** %stackaddr$prim56567, align 8
%clofunc56568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48108)
musttail call tailcc void %clofunc56568(%struct.ScmObj* %u48108, %struct.ScmObj* %argslist55191$u481082)
ret void
}

define tailcc void @proc_clo$ae50333(%struct.ScmObj* %env$ae50333,%struct.ScmObj* %current_45args55194) {
%stackaddr$prim56569 = alloca %struct.ScmObj*, align 8
%k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55194)
store volatile %struct.ScmObj* %k48397, %struct.ScmObj** %stackaddr$prim56569, align 8
%stackaddr$prim56570 = alloca %struct.ScmObj*, align 8
%current_45args55195 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55194)
store volatile %struct.ScmObj* %current_45args55195, %struct.ScmObj** %stackaddr$prim56570, align 8
%stackaddr$prim56571 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55195)
store volatile %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$prim56571, align 8
%ae50334 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56572 = alloca %struct.ScmObj*, align 8
%a48113 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50334, %struct.ScmObj* %a48112)
store volatile %struct.ScmObj* %a48113, %struct.ScmObj** %stackaddr$prim56572, align 8
%stackaddr$makeclosure56573 = alloca %struct.ScmObj*, align 8
%fptrToInt56574 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50336 to i64
%ae50336 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56574)
store volatile %struct.ScmObj* %ae50336, %struct.ScmObj** %stackaddr$makeclosure56573, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50336, %struct.ScmObj* %a48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50336, %struct.ScmObj* %k48397, i64 1)
%ae50337 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56575 = alloca %struct.ScmObj*, align 8
%fptrToInt56576 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50338 to i64
%ae50338 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56576)
store volatile %struct.ScmObj* %ae50338, %struct.ScmObj** %stackaddr$makeclosure56575, align 8
%argslist55217$ae503360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56577 = alloca %struct.ScmObj*, align 8
%argslist55217$ae503361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50338, %struct.ScmObj* %argslist55217$ae503360)
store volatile %struct.ScmObj* %argslist55217$ae503361, %struct.ScmObj** %stackaddr$prim56577, align 8
%stackaddr$prim56578 = alloca %struct.ScmObj*, align 8
%argslist55217$ae503362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50337, %struct.ScmObj* %argslist55217$ae503361)
store volatile %struct.ScmObj* %argslist55217$ae503362, %struct.ScmObj** %stackaddr$prim56578, align 8
%clofunc56579 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50336)
musttail call tailcc void %clofunc56579(%struct.ScmObj* %ae50336, %struct.ScmObj* %argslist55217$ae503362)
ret void
}

define tailcc void @proc_clo$ae50336(%struct.ScmObj* %env$ae50336,%struct.ScmObj* %current_45args55197) {
%stackaddr$env-ref56580 = alloca %struct.ScmObj*, align 8
%a48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50336, i64 0)
store %struct.ScmObj* %a48113, %struct.ScmObj** %stackaddr$env-ref56580
%stackaddr$env-ref56581 = alloca %struct.ScmObj*, align 8
%k48397 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50336, i64 1)
store %struct.ScmObj* %k48397, %struct.ScmObj** %stackaddr$env-ref56581
%stackaddr$prim56582 = alloca %struct.ScmObj*, align 8
%_95k48398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55197)
store volatile %struct.ScmObj* %_95k48398, %struct.ScmObj** %stackaddr$prim56582, align 8
%stackaddr$prim56583 = alloca %struct.ScmObj*, align 8
%current_45args55198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55197)
store volatile %struct.ScmObj* %current_45args55198, %struct.ScmObj** %stackaddr$prim56583, align 8
%stackaddr$prim56584 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55198)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim56584, align 8
%stackaddr$makeclosure56585 = alloca %struct.ScmObj*, align 8
%fptrToInt56586 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50355 to i64
%ae50355 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56586)
store volatile %struct.ScmObj* %ae50355, %struct.ScmObj** %stackaddr$makeclosure56585, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50355, %struct.ScmObj* %a48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50355, %struct.ScmObj* %k48397, i64 1)
%stackaddr$makeclosure56587 = alloca %struct.ScmObj*, align 8
%fptrToInt56588 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50356 to i64
%ae50356 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56588)
store volatile %struct.ScmObj* %ae50356, %struct.ScmObj** %stackaddr$makeclosure56587, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50356, %struct.ScmObj* %a48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50356, %struct.ScmObj* %k48397, i64 1)
%argslist55212$anf_45bind482400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56589 = alloca %struct.ScmObj*, align 8
%argslist55212$anf_45bind482401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50356, %struct.ScmObj* %argslist55212$anf_45bind482400)
store volatile %struct.ScmObj* %argslist55212$anf_45bind482401, %struct.ScmObj** %stackaddr$prim56589, align 8
%stackaddr$prim56590 = alloca %struct.ScmObj*, align 8
%argslist55212$anf_45bind482402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50355, %struct.ScmObj* %argslist55212$anf_45bind482401)
store volatile %struct.ScmObj* %argslist55212$anf_45bind482402, %struct.ScmObj** %stackaddr$prim56590, align 8
%clofunc56591 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48240)
musttail call tailcc void %clofunc56591(%struct.ScmObj* %anf_45bind48240, %struct.ScmObj* %argslist55212$anf_45bind482402)
ret void
}

define tailcc void @proc_clo$ae50355(%struct.ScmObj* %env$ae50355,%struct.ScmObj* %current_45args55200) {
%stackaddr$env-ref56592 = alloca %struct.ScmObj*, align 8
%a48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50355, i64 0)
store %struct.ScmObj* %a48113, %struct.ScmObj** %stackaddr$env-ref56592
%stackaddr$env-ref56593 = alloca %struct.ScmObj*, align 8
%k48397 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50355, i64 1)
store %struct.ScmObj* %k48397, %struct.ScmObj** %stackaddr$env-ref56593
%stackaddr$prim56594 = alloca %struct.ScmObj*, align 8
%_95k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55200)
store volatile %struct.ScmObj* %_95k48399, %struct.ScmObj** %stackaddr$prim56594, align 8
%stackaddr$prim56595 = alloca %struct.ScmObj*, align 8
%current_45args55201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55200)
store volatile %struct.ScmObj* %current_45args55201, %struct.ScmObj** %stackaddr$prim56595, align 8
%stackaddr$prim56596 = alloca %struct.ScmObj*, align 8
%cc48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55201)
store volatile %struct.ScmObj* %cc48114, %struct.ScmObj** %stackaddr$prim56596, align 8
%ae50471 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56597 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50471)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim56597, align 8
%stackaddr$prim56598 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim56598, align 8
%truthy$cmp56599 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48242)
%cmp$cmp56599 = icmp eq i64 %truthy$cmp56599, 1
br i1 %cmp$cmp56599, label %truebranch$cmp56599, label %falsebranch$cmp56599
truebranch$cmp56599:
%ae50475 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50476 = call %struct.ScmObj* @const_init_true()
%argslist55203$k483970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56600 = alloca %struct.ScmObj*, align 8
%argslist55203$k483971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50476, %struct.ScmObj* %argslist55203$k483970)
store volatile %struct.ScmObj* %argslist55203$k483971, %struct.ScmObj** %stackaddr$prim56600, align 8
%stackaddr$prim56601 = alloca %struct.ScmObj*, align 8
%argslist55203$k483972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50475, %struct.ScmObj* %argslist55203$k483971)
store volatile %struct.ScmObj* %argslist55203$k483972, %struct.ScmObj** %stackaddr$prim56601, align 8
%clofunc56602 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48397)
musttail call tailcc void %clofunc56602(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist55203$k483972)
ret void
falsebranch$cmp56599:
%ae50484 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56603 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50484)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim56603, align 8
%stackaddr$prim56604 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48243)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim56604, align 8
%truthy$cmp56605 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48244)
%cmp$cmp56605 = icmp eq i64 %truthy$cmp56605, 1
br i1 %cmp$cmp56605, label %truebranch$cmp56605, label %falsebranch$cmp56605
truebranch$cmp56605:
%ae50488 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56606 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50488)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim56606, align 8
%stackaddr$prim56607 = alloca %struct.ScmObj*, align 8
%b48116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %b48116, %struct.ScmObj** %stackaddr$prim56607, align 8
%ae50491 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56608 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50491)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim56608, align 8
%stackaddr$prim56609 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim56609, align 8
%ae50494 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56610 = alloca %struct.ScmObj*, align 8
%_95048117 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50494, %struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %_95048117, %struct.ScmObj** %stackaddr$prim56610, align 8
%argslist55204$cc481140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56611 = alloca %struct.ScmObj*, align 8
%argslist55204$cc481141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist55204$cc481140)
store volatile %struct.ScmObj* %argslist55204$cc481141, %struct.ScmObj** %stackaddr$prim56611, align 8
%stackaddr$prim56612 = alloca %struct.ScmObj*, align 8
%argslist55204$cc481142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist55204$cc481141)
store volatile %struct.ScmObj* %argslist55204$cc481142, %struct.ScmObj** %stackaddr$prim56612, align 8
%clofunc56613 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48114)
musttail call tailcc void %clofunc56613(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist55204$cc481142)
ret void
falsebranch$cmp56605:
%ae50527 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50528 = call %struct.ScmObj* @const_init_false()
%argslist55205$k483970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56614 = alloca %struct.ScmObj*, align 8
%argslist55205$k483971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50528, %struct.ScmObj* %argslist55205$k483970)
store volatile %struct.ScmObj* %argslist55205$k483971, %struct.ScmObj** %stackaddr$prim56614, align 8
%stackaddr$prim56615 = alloca %struct.ScmObj*, align 8
%argslist55205$k483972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50527, %struct.ScmObj* %argslist55205$k483971)
store volatile %struct.ScmObj* %argslist55205$k483972, %struct.ScmObj** %stackaddr$prim56615, align 8
%clofunc56616 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48397)
musttail call tailcc void %clofunc56616(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist55205$k483972)
ret void
}

define tailcc void @proc_clo$ae50356(%struct.ScmObj* %env$ae50356,%struct.ScmObj* %current_45args55206) {
%stackaddr$env-ref56617 = alloca %struct.ScmObj*, align 8
%a48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50356, i64 0)
store %struct.ScmObj* %a48113, %struct.ScmObj** %stackaddr$env-ref56617
%stackaddr$env-ref56618 = alloca %struct.ScmObj*, align 8
%k48397 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50356, i64 1)
store %struct.ScmObj* %k48397, %struct.ScmObj** %stackaddr$env-ref56618
%stackaddr$prim56619 = alloca %struct.ScmObj*, align 8
%_95k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55206)
store volatile %struct.ScmObj* %_95k48399, %struct.ScmObj** %stackaddr$prim56619, align 8
%stackaddr$prim56620 = alloca %struct.ScmObj*, align 8
%current_45args55207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55206)
store volatile %struct.ScmObj* %current_45args55207, %struct.ScmObj** %stackaddr$prim56620, align 8
%stackaddr$prim56621 = alloca %struct.ScmObj*, align 8
%cc48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55207)
store volatile %struct.ScmObj* %cc48114, %struct.ScmObj** %stackaddr$prim56621, align 8
%ae50358 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56622 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50358)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim56622, align 8
%stackaddr$prim56623 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim56623, align 8
%truthy$cmp56624 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48242)
%cmp$cmp56624 = icmp eq i64 %truthy$cmp56624, 1
br i1 %cmp$cmp56624, label %truebranch$cmp56624, label %falsebranch$cmp56624
truebranch$cmp56624:
%ae50362 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50363 = call %struct.ScmObj* @const_init_true()
%argslist55209$k483970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56625 = alloca %struct.ScmObj*, align 8
%argslist55209$k483971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50363, %struct.ScmObj* %argslist55209$k483970)
store volatile %struct.ScmObj* %argslist55209$k483971, %struct.ScmObj** %stackaddr$prim56625, align 8
%stackaddr$prim56626 = alloca %struct.ScmObj*, align 8
%argslist55209$k483972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50362, %struct.ScmObj* %argslist55209$k483971)
store volatile %struct.ScmObj* %argslist55209$k483972, %struct.ScmObj** %stackaddr$prim56626, align 8
%clofunc56627 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48397)
musttail call tailcc void %clofunc56627(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist55209$k483972)
ret void
falsebranch$cmp56624:
%ae50371 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56628 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50371)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim56628, align 8
%stackaddr$prim56629 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48243)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim56629, align 8
%truthy$cmp56630 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48244)
%cmp$cmp56630 = icmp eq i64 %truthy$cmp56630, 1
br i1 %cmp$cmp56630, label %truebranch$cmp56630, label %falsebranch$cmp56630
truebranch$cmp56630:
%ae50375 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56631 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50375)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim56631, align 8
%stackaddr$prim56632 = alloca %struct.ScmObj*, align 8
%b48116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %b48116, %struct.ScmObj** %stackaddr$prim56632, align 8
%ae50378 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56633 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50378)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim56633, align 8
%stackaddr$prim56634 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim56634, align 8
%ae50381 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56635 = alloca %struct.ScmObj*, align 8
%_95048117 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50381, %struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %_95048117, %struct.ScmObj** %stackaddr$prim56635, align 8
%argslist55210$cc481140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56636 = alloca %struct.ScmObj*, align 8
%argslist55210$cc481141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist55210$cc481140)
store volatile %struct.ScmObj* %argslist55210$cc481141, %struct.ScmObj** %stackaddr$prim56636, align 8
%stackaddr$prim56637 = alloca %struct.ScmObj*, align 8
%argslist55210$cc481142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist55210$cc481141)
store volatile %struct.ScmObj* %argslist55210$cc481142, %struct.ScmObj** %stackaddr$prim56637, align 8
%clofunc56638 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48114)
musttail call tailcc void %clofunc56638(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist55210$cc481142)
ret void
falsebranch$cmp56630:
%ae50414 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50415 = call %struct.ScmObj* @const_init_false()
%argslist55211$k483970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56639 = alloca %struct.ScmObj*, align 8
%argslist55211$k483971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50415, %struct.ScmObj* %argslist55211$k483970)
store volatile %struct.ScmObj* %argslist55211$k483971, %struct.ScmObj** %stackaddr$prim56639, align 8
%stackaddr$prim56640 = alloca %struct.ScmObj*, align 8
%argslist55211$k483972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50414, %struct.ScmObj* %argslist55211$k483971)
store volatile %struct.ScmObj* %argslist55211$k483972, %struct.ScmObj** %stackaddr$prim56640, align 8
%clofunc56641 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48397)
musttail call tailcc void %clofunc56641(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist55211$k483972)
ret void
}

define tailcc void @proc_clo$ae50338(%struct.ScmObj* %env$ae50338,%struct.ScmObj* %current_45args55213) {
%stackaddr$prim56642 = alloca %struct.ScmObj*, align 8
%k48400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55213)
store volatile %struct.ScmObj* %k48400, %struct.ScmObj** %stackaddr$prim56642, align 8
%stackaddr$prim56643 = alloca %struct.ScmObj*, align 8
%current_45args55214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55213)
store volatile %struct.ScmObj* %current_45args55214, %struct.ScmObj** %stackaddr$prim56643, align 8
%stackaddr$prim56644 = alloca %struct.ScmObj*, align 8
%k48115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55214)
store volatile %struct.ScmObj* %k48115, %struct.ScmObj** %stackaddr$prim56644, align 8
%ae50340 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55216$k484000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56645 = alloca %struct.ScmObj*, align 8
%argslist55216$k484001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48115, %struct.ScmObj* %argslist55216$k484000)
store volatile %struct.ScmObj* %argslist55216$k484001, %struct.ScmObj** %stackaddr$prim56645, align 8
%stackaddr$prim56646 = alloca %struct.ScmObj*, align 8
%argslist55216$k484002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50340, %struct.ScmObj* %argslist55216$k484001)
store volatile %struct.ScmObj* %argslist55216$k484002, %struct.ScmObj** %stackaddr$prim56646, align 8
%clofunc56647 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48400)
musttail call tailcc void %clofunc56647(%struct.ScmObj* %k48400, %struct.ScmObj* %argslist55216$k484002)
ret void
}

define tailcc void @proc_clo$ae50261(%struct.ScmObj* %env$ae50261,%struct.ScmObj* %current_45args55219) {
%stackaddr$env-ref56648 = alloca %struct.ScmObj*, align 8
%_37append48119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50261, i64 0)
store %struct.ScmObj* %_37append48119, %struct.ScmObj** %stackaddr$env-ref56648
%stackaddr$prim56649 = alloca %struct.ScmObj*, align 8
%k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55219)
store volatile %struct.ScmObj* %k48401, %struct.ScmObj** %stackaddr$prim56649, align 8
%stackaddr$prim56650 = alloca %struct.ScmObj*, align 8
%current_45args55220 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55219)
store volatile %struct.ScmObj* %current_45args55220, %struct.ScmObj** %stackaddr$prim56650, align 8
%stackaddr$prim56651 = alloca %struct.ScmObj*, align 8
%ls048122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55220)
store volatile %struct.ScmObj* %ls048122, %struct.ScmObj** %stackaddr$prim56651, align 8
%stackaddr$prim56652 = alloca %struct.ScmObj*, align 8
%current_45args55221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55220)
store volatile %struct.ScmObj* %current_45args55221, %struct.ScmObj** %stackaddr$prim56652, align 8
%stackaddr$prim56653 = alloca %struct.ScmObj*, align 8
%ls148121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55221)
store volatile %struct.ScmObj* %ls148121, %struct.ScmObj** %stackaddr$prim56653, align 8
%stackaddr$prim56654 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048122)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim56654, align 8
%truthy$cmp56655 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48234)
%cmp$cmp56655 = icmp eq i64 %truthy$cmp56655, 1
br i1 %cmp$cmp56655, label %truebranch$cmp56655, label %falsebranch$cmp56655
truebranch$cmp56655:
%ae50265 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55223$k484010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56656 = alloca %struct.ScmObj*, align 8
%argslist55223$k484011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148121, %struct.ScmObj* %argslist55223$k484010)
store volatile %struct.ScmObj* %argslist55223$k484011, %struct.ScmObj** %stackaddr$prim56656, align 8
%stackaddr$prim56657 = alloca %struct.ScmObj*, align 8
%argslist55223$k484012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50265, %struct.ScmObj* %argslist55223$k484011)
store volatile %struct.ScmObj* %argslist55223$k484012, %struct.ScmObj** %stackaddr$prim56657, align 8
%clofunc56658 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48401)
musttail call tailcc void %clofunc56658(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist55223$k484012)
ret void
falsebranch$cmp56655:
%stackaddr$prim56659 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048122)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim56659, align 8
%ae50272 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56660 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48119, %struct.ScmObj* %ae50272)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim56660, align 8
%stackaddr$prim56661 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048122)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim56661, align 8
%stackaddr$makeclosure56662 = alloca %struct.ScmObj*, align 8
%fptrToInt56663 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50275 to i64
%ae50275 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56663)
store volatile %struct.ScmObj* %ae50275, %struct.ScmObj** %stackaddr$makeclosure56662, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50275, %struct.ScmObj* %k48401, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50275, %struct.ScmObj* %anf_45bind48235, i64 1)
%argslist55228$anf_45bind482360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56664 = alloca %struct.ScmObj*, align 8
%argslist55228$anf_45bind482361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148121, %struct.ScmObj* %argslist55228$anf_45bind482360)
store volatile %struct.ScmObj* %argslist55228$anf_45bind482361, %struct.ScmObj** %stackaddr$prim56664, align 8
%stackaddr$prim56665 = alloca %struct.ScmObj*, align 8
%argslist55228$anf_45bind482362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48237, %struct.ScmObj* %argslist55228$anf_45bind482361)
store volatile %struct.ScmObj* %argslist55228$anf_45bind482362, %struct.ScmObj** %stackaddr$prim56665, align 8
%stackaddr$prim56666 = alloca %struct.ScmObj*, align 8
%argslist55228$anf_45bind482363 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50275, %struct.ScmObj* %argslist55228$anf_45bind482362)
store volatile %struct.ScmObj* %argslist55228$anf_45bind482363, %struct.ScmObj** %stackaddr$prim56666, align 8
%clofunc56667 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48236)
musttail call tailcc void %clofunc56667(%struct.ScmObj* %anf_45bind48236, %struct.ScmObj* %argslist55228$anf_45bind482363)
ret void
}

define tailcc void @proc_clo$ae50275(%struct.ScmObj* %env$ae50275,%struct.ScmObj* %current_45args55224) {
%stackaddr$env-ref56668 = alloca %struct.ScmObj*, align 8
%k48401 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50275, i64 0)
store %struct.ScmObj* %k48401, %struct.ScmObj** %stackaddr$env-ref56668
%stackaddr$env-ref56669 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50275, i64 1)
store %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$env-ref56669
%stackaddr$prim56670 = alloca %struct.ScmObj*, align 8
%_95k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55224)
store volatile %struct.ScmObj* %_95k48402, %struct.ScmObj** %stackaddr$prim56670, align 8
%stackaddr$prim56671 = alloca %struct.ScmObj*, align 8
%current_45args55225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55224)
store volatile %struct.ScmObj* %current_45args55225, %struct.ScmObj** %stackaddr$prim56671, align 8
%stackaddr$prim56672 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55225)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim56672, align 8
%stackaddr$prim56673 = alloca %struct.ScmObj*, align 8
%cpsprim48403 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48235, %struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %cpsprim48403, %struct.ScmObj** %stackaddr$prim56673, align 8
%ae50281 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55227$k484010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56674 = alloca %struct.ScmObj*, align 8
%argslist55227$k484011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48403, %struct.ScmObj* %argslist55227$k484010)
store volatile %struct.ScmObj* %argslist55227$k484011, %struct.ScmObj** %stackaddr$prim56674, align 8
%stackaddr$prim56675 = alloca %struct.ScmObj*, align 8
%argslist55227$k484012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50281, %struct.ScmObj* %argslist55227$k484011)
store volatile %struct.ScmObj* %argslist55227$k484012, %struct.ScmObj** %stackaddr$prim56675, align 8
%clofunc56676 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48401)
musttail call tailcc void %clofunc56676(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist55227$k484012)
ret void
}

define tailcc void @proc_clo$ae50235(%struct.ScmObj* %env$ae50235,%struct.ScmObj* %current_45args55230) {
%stackaddr$prim56677 = alloca %struct.ScmObj*, align 8
%k48404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55230)
store volatile %struct.ScmObj* %k48404, %struct.ScmObj** %stackaddr$prim56677, align 8
%stackaddr$prim56678 = alloca %struct.ScmObj*, align 8
%current_45args55231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55230)
store volatile %struct.ScmObj* %current_45args55231, %struct.ScmObj** %stackaddr$prim56678, align 8
%stackaddr$prim56679 = alloca %struct.ScmObj*, align 8
%a48125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55231)
store volatile %struct.ScmObj* %a48125, %struct.ScmObj** %stackaddr$prim56679, align 8
%stackaddr$prim56680 = alloca %struct.ScmObj*, align 8
%current_45args55232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55231)
store volatile %struct.ScmObj* %current_45args55232, %struct.ScmObj** %stackaddr$prim56680, align 8
%stackaddr$prim56681 = alloca %struct.ScmObj*, align 8
%b48124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55232)
store volatile %struct.ScmObj* %b48124, %struct.ScmObj** %stackaddr$prim56681, align 8
%stackaddr$prim56682 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48125, %struct.ScmObj* %b48124)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim56682, align 8
%stackaddr$prim56683 = alloca %struct.ScmObj*, align 8
%cpsprim48405 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %cpsprim48405, %struct.ScmObj** %stackaddr$prim56683, align 8
%ae50240 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55234$k484040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56684 = alloca %struct.ScmObj*, align 8
%argslist55234$k484041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48405, %struct.ScmObj* %argslist55234$k484040)
store volatile %struct.ScmObj* %argslist55234$k484041, %struct.ScmObj** %stackaddr$prim56684, align 8
%stackaddr$prim56685 = alloca %struct.ScmObj*, align 8
%argslist55234$k484042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50240, %struct.ScmObj* %argslist55234$k484041)
store volatile %struct.ScmObj* %argslist55234$k484042, %struct.ScmObj** %stackaddr$prim56685, align 8
%clofunc56686 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48404)
musttail call tailcc void %clofunc56686(%struct.ScmObj* %k48404, %struct.ScmObj* %argslist55234$k484042)
ret void
}

define tailcc void @proc_clo$ae50211(%struct.ScmObj* %env$ae50211,%struct.ScmObj* %current_45args55236) {
%stackaddr$prim56687 = alloca %struct.ScmObj*, align 8
%k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55236)
store volatile %struct.ScmObj* %k48406, %struct.ScmObj** %stackaddr$prim56687, align 8
%stackaddr$prim56688 = alloca %struct.ScmObj*, align 8
%current_45args55237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55236)
store volatile %struct.ScmObj* %current_45args55237, %struct.ScmObj** %stackaddr$prim56688, align 8
%stackaddr$prim56689 = alloca %struct.ScmObj*, align 8
%a48128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55237)
store volatile %struct.ScmObj* %a48128, %struct.ScmObj** %stackaddr$prim56689, align 8
%stackaddr$prim56690 = alloca %struct.ScmObj*, align 8
%current_45args55238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55237)
store volatile %struct.ScmObj* %current_45args55238, %struct.ScmObj** %stackaddr$prim56690, align 8
%stackaddr$prim56691 = alloca %struct.ScmObj*, align 8
%b48127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55238)
store volatile %struct.ScmObj* %b48127, %struct.ScmObj** %stackaddr$prim56691, align 8
%stackaddr$prim56692 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48128, %struct.ScmObj* %b48127)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim56692, align 8
%stackaddr$prim56693 = alloca %struct.ScmObj*, align 8
%cpsprim48407 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %cpsprim48407, %struct.ScmObj** %stackaddr$prim56693, align 8
%ae50216 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55240$k484060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56694 = alloca %struct.ScmObj*, align 8
%argslist55240$k484061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48407, %struct.ScmObj* %argslist55240$k484060)
store volatile %struct.ScmObj* %argslist55240$k484061, %struct.ScmObj** %stackaddr$prim56694, align 8
%stackaddr$prim56695 = alloca %struct.ScmObj*, align 8
%argslist55240$k484062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50216, %struct.ScmObj* %argslist55240$k484061)
store volatile %struct.ScmObj* %argslist55240$k484062, %struct.ScmObj** %stackaddr$prim56695, align 8
%clofunc56696 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48406)
musttail call tailcc void %clofunc56696(%struct.ScmObj* %k48406, %struct.ScmObj* %argslist55240$k484062)
ret void
}

define tailcc void @proc_clo$ae49817(%struct.ScmObj* %env$ae49817,%struct.ScmObj* %current_45args55243) {
%stackaddr$env-ref56697 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49817, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56697
%stackaddr$env-ref56698 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49817, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56698
%stackaddr$env-ref56699 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49817, i64 2)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref56699
%stackaddr$prim56700 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55243)
store volatile %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$prim56700, align 8
%stackaddr$prim56701 = alloca %struct.ScmObj*, align 8
%current_45args55244 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55243)
store volatile %struct.ScmObj* %current_45args55244, %struct.ScmObj** %stackaddr$prim56701, align 8
%stackaddr$prim56702 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55244)
store volatile %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$prim56702, align 8
%ae49819 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56703 = alloca %struct.ScmObj*, align 8
%fptrToInt56704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49820 to i64
%ae49820 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56704)
store volatile %struct.ScmObj* %ae49820, %struct.ScmObj** %stackaddr$makeclosure56703, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %_37foldl48130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %_37foldr148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %_37map148078, i64 3)
%argslist55301$k484080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56705 = alloca %struct.ScmObj*, align 8
%argslist55301$k484081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49820, %struct.ScmObj* %argslist55301$k484080)
store volatile %struct.ScmObj* %argslist55301$k484081, %struct.ScmObj** %stackaddr$prim56705, align 8
%stackaddr$prim56706 = alloca %struct.ScmObj*, align 8
%argslist55301$k484082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49819, %struct.ScmObj* %argslist55301$k484081)
store volatile %struct.ScmObj* %argslist55301$k484082, %struct.ScmObj** %stackaddr$prim56706, align 8
%clofunc56707 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48408)
musttail call tailcc void %clofunc56707(%struct.ScmObj* %k48408, %struct.ScmObj* %argslist55301$k484082)
ret void
}

define tailcc void @proc_clo$ae49820(%struct.ScmObj* %env$ae49820,%struct.ScmObj* %args4813148409) {
%stackaddr$env-ref56708 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56708
%stackaddr$env-ref56709 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 1)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref56709
%stackaddr$env-ref56710 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 2)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56710
%stackaddr$env-ref56711 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 3)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref56711
%stackaddr$prim56712 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813148409)
store volatile %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$prim56712, align 8
%stackaddr$prim56713 = alloca %struct.ScmObj*, align 8
%args48131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813148409)
store volatile %struct.ScmObj* %args48131, %struct.ScmObj** %stackaddr$prim56713, align 8
%stackaddr$prim56714 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48131)
store volatile %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$prim56714, align 8
%stackaddr$prim56715 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48131)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim56715, align 8
%stackaddr$prim56716 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48220)
store volatile %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$prim56716, align 8
%stackaddr$prim56717 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48131)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim56717, align 8
%stackaddr$prim56718 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48221)
store volatile %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$prim56718, align 8
%stackaddr$makeclosure56719 = alloca %struct.ScmObj*, align 8
%fptrToInt56720 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49828 to i64
%ae49828 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56720)
store volatile %struct.ScmObj* %ae49828, %struct.ScmObj** %stackaddr$makeclosure56719, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49828, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49828, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49828, %struct.ScmObj* %f48134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49828, %struct.ScmObj* %acc48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49828, %struct.ScmObj* %_37foldl48130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49828, %struct.ScmObj* %_37foldr148047, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49828, %struct.ScmObj* %_37map148078, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49828, %struct.ScmObj* %k48410, i64 7)
%ae49829 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56721 = alloca %struct.ScmObj*, align 8
%fptrToInt56722 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49830 to i64
%ae49830 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56722)
store volatile %struct.ScmObj* %ae49830, %struct.ScmObj** %stackaddr$makeclosure56721, align 8
%argslist55300$ae498280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56723 = alloca %struct.ScmObj*, align 8
%argslist55300$ae498281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49830, %struct.ScmObj* %argslist55300$ae498280)
store volatile %struct.ScmObj* %argslist55300$ae498281, %struct.ScmObj** %stackaddr$prim56723, align 8
%stackaddr$prim56724 = alloca %struct.ScmObj*, align 8
%argslist55300$ae498282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49829, %struct.ScmObj* %argslist55300$ae498281)
store volatile %struct.ScmObj* %argslist55300$ae498282, %struct.ScmObj** %stackaddr$prim56724, align 8
%clofunc56725 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49828)
musttail call tailcc void %clofunc56725(%struct.ScmObj* %ae49828, %struct.ScmObj* %argslist55300$ae498282)
ret void
}

define tailcc void @proc_clo$ae49828(%struct.ScmObj* %env$ae49828,%struct.ScmObj* %current_45args55246) {
%stackaddr$env-ref56726 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49828, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref56726
%stackaddr$env-ref56727 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49828, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56727
%stackaddr$env-ref56728 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49828, i64 2)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref56728
%stackaddr$env-ref56729 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49828, i64 3)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref56729
%stackaddr$env-ref56730 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49828, i64 4)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref56730
%stackaddr$env-ref56731 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49828, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56731
%stackaddr$env-ref56732 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49828, i64 6)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref56732
%stackaddr$env-ref56733 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49828, i64 7)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref56733
%stackaddr$prim56734 = alloca %struct.ScmObj*, align 8
%_95k48411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55246)
store volatile %struct.ScmObj* %_95k48411, %struct.ScmObj** %stackaddr$prim56734, align 8
%stackaddr$prim56735 = alloca %struct.ScmObj*, align 8
%current_45args55247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55246)
store volatile %struct.ScmObj* %current_45args55247, %struct.ScmObj** %stackaddr$prim56735, align 8
%stackaddr$prim56736 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55247)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim56736, align 8
%stackaddr$makeclosure56737 = alloca %struct.ScmObj*, align 8
%fptrToInt56738 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49860 to i64
%ae49860 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56738)
store volatile %struct.ScmObj* %ae49860, %struct.ScmObj** %stackaddr$makeclosure56737, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %f48134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %acc48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %_37foldl48130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %_37map148078, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %k48410, i64 6)
%ae49862 = call %struct.ScmObj* @const_init_false()
%argslist55293$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56739 = alloca %struct.ScmObj*, align 8
%argslist55293$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48132, %struct.ScmObj* %argslist55293$_37foldr1480470)
store volatile %struct.ScmObj* %argslist55293$_37foldr1480471, %struct.ScmObj** %stackaddr$prim56739, align 8
%stackaddr$prim56740 = alloca %struct.ScmObj*, align 8
%argslist55293$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49862, %struct.ScmObj* %argslist55293$_37foldr1480471)
store volatile %struct.ScmObj* %argslist55293$_37foldr1480472, %struct.ScmObj** %stackaddr$prim56740, align 8
%stackaddr$prim56741 = alloca %struct.ScmObj*, align 8
%argslist55293$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48222, %struct.ScmObj* %argslist55293$_37foldr1480472)
store volatile %struct.ScmObj* %argslist55293$_37foldr1480473, %struct.ScmObj** %stackaddr$prim56741, align 8
%stackaddr$prim56742 = alloca %struct.ScmObj*, align 8
%argslist55293$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49860, %struct.ScmObj* %argslist55293$_37foldr1480473)
store volatile %struct.ScmObj* %argslist55293$_37foldr1480474, %struct.ScmObj** %stackaddr$prim56742, align 8
%clofunc56743 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc56743(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist55293$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae49860(%struct.ScmObj* %env$ae49860,%struct.ScmObj* %current_45args55249) {
%stackaddr$env-ref56744 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref56744
%stackaddr$env-ref56745 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56745
%stackaddr$env-ref56746 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 2)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref56746
%stackaddr$env-ref56747 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 3)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref56747
%stackaddr$env-ref56748 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 4)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref56748
%stackaddr$env-ref56749 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 5)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref56749
%stackaddr$env-ref56750 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 6)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref56750
%stackaddr$prim56751 = alloca %struct.ScmObj*, align 8
%_95k48412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55249)
store volatile %struct.ScmObj* %_95k48412, %struct.ScmObj** %stackaddr$prim56751, align 8
%stackaddr$prim56752 = alloca %struct.ScmObj*, align 8
%current_45args55250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55249)
store volatile %struct.ScmObj* %current_45args55250, %struct.ScmObj** %stackaddr$prim56752, align 8
%stackaddr$prim56753 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55250)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim56753, align 8
%truthy$cmp56754 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48223)
%cmp$cmp56754 = icmp eq i64 %truthy$cmp56754, 1
br i1 %cmp$cmp56754, label %truebranch$cmp56754, label %falsebranch$cmp56754
truebranch$cmp56754:
%ae49871 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55252$k484100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56755 = alloca %struct.ScmObj*, align 8
%argslist55252$k484101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48133, %struct.ScmObj* %argslist55252$k484100)
store volatile %struct.ScmObj* %argslist55252$k484101, %struct.ScmObj** %stackaddr$prim56755, align 8
%stackaddr$prim56756 = alloca %struct.ScmObj*, align 8
%argslist55252$k484102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49871, %struct.ScmObj* %argslist55252$k484101)
store volatile %struct.ScmObj* %argslist55252$k484102, %struct.ScmObj** %stackaddr$prim56756, align 8
%clofunc56757 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48410)
musttail call tailcc void %clofunc56757(%struct.ScmObj* %k48410, %struct.ScmObj* %argslist55252$k484102)
ret void
falsebranch$cmp56754:
%stackaddr$makeclosure56758 = alloca %struct.ScmObj*, align 8
%fptrToInt56759 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49876 to i64
%ae49876 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56759)
store volatile %struct.ScmObj* %ae49876, %struct.ScmObj** %stackaddr$makeclosure56758, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49876, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49876, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49876, %struct.ScmObj* %f48134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49876, %struct.ScmObj* %acc48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49876, %struct.ScmObj* %_37foldl48130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49876, %struct.ScmObj* %_37map148078, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49876, %struct.ScmObj* %k48410, i64 6)
%ae49877 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56760 = alloca %struct.ScmObj*, align 8
%fptrToInt56761 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49878 to i64
%ae49878 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56761)
store volatile %struct.ScmObj* %ae49878, %struct.ScmObj** %stackaddr$makeclosure56760, align 8
%argslist55292$ae498760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56762 = alloca %struct.ScmObj*, align 8
%argslist55292$ae498761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49878, %struct.ScmObj* %argslist55292$ae498760)
store volatile %struct.ScmObj* %argslist55292$ae498761, %struct.ScmObj** %stackaddr$prim56762, align 8
%stackaddr$prim56763 = alloca %struct.ScmObj*, align 8
%argslist55292$ae498762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49877, %struct.ScmObj* %argslist55292$ae498761)
store volatile %struct.ScmObj* %argslist55292$ae498762, %struct.ScmObj** %stackaddr$prim56763, align 8
%clofunc56764 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49876)
musttail call tailcc void %clofunc56764(%struct.ScmObj* %ae49876, %struct.ScmObj* %argslist55292$ae498762)
ret void
}

define tailcc void @proc_clo$ae49876(%struct.ScmObj* %env$ae49876,%struct.ScmObj* %current_45args55253) {
%stackaddr$env-ref56765 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49876, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref56765
%stackaddr$env-ref56766 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49876, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56766
%stackaddr$env-ref56767 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49876, i64 2)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref56767
%stackaddr$env-ref56768 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49876, i64 3)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref56768
%stackaddr$env-ref56769 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49876, i64 4)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref56769
%stackaddr$env-ref56770 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49876, i64 5)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref56770
%stackaddr$env-ref56771 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49876, i64 6)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref56771
%stackaddr$prim56772 = alloca %struct.ScmObj*, align 8
%_95k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55253)
store volatile %struct.ScmObj* %_95k48413, %struct.ScmObj** %stackaddr$prim56772, align 8
%stackaddr$prim56773 = alloca %struct.ScmObj*, align 8
%current_45args55254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55253)
store volatile %struct.ScmObj* %current_45args55254, %struct.ScmObj** %stackaddr$prim56773, align 8
%stackaddr$prim56774 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55254)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim56774, align 8
%stackaddr$makeclosure56775 = alloca %struct.ScmObj*, align 8
%fptrToInt56776 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49897 to i64
%ae49897 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56776)
store volatile %struct.ScmObj* %ae49897, %struct.ScmObj** %stackaddr$makeclosure56775, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %f48134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %acc48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %_37foldl48130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %_37map148078, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %k48410, i64 6)
%argslist55287$_37map1480780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56777 = alloca %struct.ScmObj*, align 8
%argslist55287$_37map1480781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48132, %struct.ScmObj* %argslist55287$_37map1480780)
store volatile %struct.ScmObj* %argslist55287$_37map1480781, %struct.ScmObj** %stackaddr$prim56777, align 8
%stackaddr$prim56778 = alloca %struct.ScmObj*, align 8
%argslist55287$_37map1480782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48224, %struct.ScmObj* %argslist55287$_37map1480781)
store volatile %struct.ScmObj* %argslist55287$_37map1480782, %struct.ScmObj** %stackaddr$prim56778, align 8
%stackaddr$prim56779 = alloca %struct.ScmObj*, align 8
%argslist55287$_37map1480783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49897, %struct.ScmObj* %argslist55287$_37map1480782)
store volatile %struct.ScmObj* %argslist55287$_37map1480783, %struct.ScmObj** %stackaddr$prim56779, align 8
%clofunc56780 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148078)
musttail call tailcc void %clofunc56780(%struct.ScmObj* %_37map148078, %struct.ScmObj* %argslist55287$_37map1480783)
ret void
}

define tailcc void @proc_clo$ae49897(%struct.ScmObj* %env$ae49897,%struct.ScmObj* %current_45args55256) {
%stackaddr$env-ref56781 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref56781
%stackaddr$env-ref56782 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56782
%stackaddr$env-ref56783 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 2)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref56783
%stackaddr$env-ref56784 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 3)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref56784
%stackaddr$env-ref56785 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 4)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref56785
%stackaddr$env-ref56786 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 5)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref56786
%stackaddr$env-ref56787 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 6)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref56787
%stackaddr$prim56788 = alloca %struct.ScmObj*, align 8
%_95k48414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55256)
store volatile %struct.ScmObj* %_95k48414, %struct.ScmObj** %stackaddr$prim56788, align 8
%stackaddr$prim56789 = alloca %struct.ScmObj*, align 8
%current_45args55257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55256)
store volatile %struct.ScmObj* %current_45args55257, %struct.ScmObj** %stackaddr$prim56789, align 8
%stackaddr$prim56790 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55257)
store volatile %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$prim56790, align 8
%stackaddr$makeclosure56791 = alloca %struct.ScmObj*, align 8
%fptrToInt56792 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49900 to i64
%ae49900 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56792)
store volatile %struct.ScmObj* %ae49900, %struct.ScmObj** %stackaddr$makeclosure56791, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49900, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49900, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49900, %struct.ScmObj* %f48134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49900, %struct.ScmObj* %acc48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49900, %struct.ScmObj* %_37foldl48130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49900, %struct.ScmObj* %_37map148078, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49900, %struct.ScmObj* %lsts_4348139, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49900, %struct.ScmObj* %k48410, i64 7)
%ae49901 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56793 = alloca %struct.ScmObj*, align 8
%fptrToInt56794 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49902 to i64
%ae49902 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56794)
store volatile %struct.ScmObj* %ae49902, %struct.ScmObj** %stackaddr$makeclosure56793, align 8
%argslist55286$ae499000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56795 = alloca %struct.ScmObj*, align 8
%argslist55286$ae499001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49902, %struct.ScmObj* %argslist55286$ae499000)
store volatile %struct.ScmObj* %argslist55286$ae499001, %struct.ScmObj** %stackaddr$prim56795, align 8
%stackaddr$prim56796 = alloca %struct.ScmObj*, align 8
%argslist55286$ae499002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49901, %struct.ScmObj* %argslist55286$ae499001)
store volatile %struct.ScmObj* %argslist55286$ae499002, %struct.ScmObj** %stackaddr$prim56796, align 8
%clofunc56797 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49900)
musttail call tailcc void %clofunc56797(%struct.ScmObj* %ae49900, %struct.ScmObj* %argslist55286$ae499002)
ret void
}

define tailcc void @proc_clo$ae49900(%struct.ScmObj* %env$ae49900,%struct.ScmObj* %current_45args55259) {
%stackaddr$env-ref56798 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49900, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref56798
%stackaddr$env-ref56799 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49900, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56799
%stackaddr$env-ref56800 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49900, i64 2)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref56800
%stackaddr$env-ref56801 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49900, i64 3)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref56801
%stackaddr$env-ref56802 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49900, i64 4)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref56802
%stackaddr$env-ref56803 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49900, i64 5)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref56803
%stackaddr$env-ref56804 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49900, i64 6)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref56804
%stackaddr$env-ref56805 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49900, i64 7)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref56805
%stackaddr$prim56806 = alloca %struct.ScmObj*, align 8
%_95k48415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55259)
store volatile %struct.ScmObj* %_95k48415, %struct.ScmObj** %stackaddr$prim56806, align 8
%stackaddr$prim56807 = alloca %struct.ScmObj*, align 8
%current_45args55260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55259)
store volatile %struct.ScmObj* %current_45args55260, %struct.ScmObj** %stackaddr$prim56807, align 8
%stackaddr$prim56808 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55260)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim56808, align 8
%stackaddr$makeclosure56809 = alloca %struct.ScmObj*, align 8
%fptrToInt56810 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49921 to i64
%ae49921 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56810)
store volatile %struct.ScmObj* %ae49921, %struct.ScmObj** %stackaddr$makeclosure56809, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49921, %struct.ScmObj* %f48134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49921, %struct.ScmObj* %acc48133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49921, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49921, %struct.ScmObj* %_37foldl48130, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49921, %struct.ScmObj* %lsts_4348139, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49921, %struct.ScmObj* %k48410, i64 5)
%argslist55281$_37map1480780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56811 = alloca %struct.ScmObj*, align 8
%argslist55281$_37map1480781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48132, %struct.ScmObj* %argslist55281$_37map1480780)
store volatile %struct.ScmObj* %argslist55281$_37map1480781, %struct.ScmObj** %stackaddr$prim56811, align 8
%stackaddr$prim56812 = alloca %struct.ScmObj*, align 8
%argslist55281$_37map1480782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48225, %struct.ScmObj* %argslist55281$_37map1480781)
store volatile %struct.ScmObj* %argslist55281$_37map1480782, %struct.ScmObj** %stackaddr$prim56812, align 8
%stackaddr$prim56813 = alloca %struct.ScmObj*, align 8
%argslist55281$_37map1480783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49921, %struct.ScmObj* %argslist55281$_37map1480782)
store volatile %struct.ScmObj* %argslist55281$_37map1480783, %struct.ScmObj** %stackaddr$prim56813, align 8
%clofunc56814 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148078)
musttail call tailcc void %clofunc56814(%struct.ScmObj* %_37map148078, %struct.ScmObj* %argslist55281$_37map1480783)
ret void
}

define tailcc void @proc_clo$ae49921(%struct.ScmObj* %env$ae49921,%struct.ScmObj* %current_45args55262) {
%stackaddr$env-ref56815 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49921, i64 0)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref56815
%stackaddr$env-ref56816 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49921, i64 1)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref56816
%stackaddr$env-ref56817 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49921, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56817
%stackaddr$env-ref56818 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49921, i64 3)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref56818
%stackaddr$env-ref56819 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49921, i64 4)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref56819
%stackaddr$env-ref56820 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49921, i64 5)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref56820
%stackaddr$prim56821 = alloca %struct.ScmObj*, align 8
%_95k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55262)
store volatile %struct.ScmObj* %_95k48416, %struct.ScmObj** %stackaddr$prim56821, align 8
%stackaddr$prim56822 = alloca %struct.ScmObj*, align 8
%current_45args55263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55262)
store volatile %struct.ScmObj* %current_45args55263, %struct.ScmObj** %stackaddr$prim56822, align 8
%stackaddr$prim56823 = alloca %struct.ScmObj*, align 8
%vs48137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55263)
store volatile %struct.ScmObj* %vs48137, %struct.ScmObj** %stackaddr$prim56823, align 8
%stackaddr$makeclosure56824 = alloca %struct.ScmObj*, align 8
%fptrToInt56825 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49924 to i64
%ae49924 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56825)
store volatile %struct.ScmObj* %ae49924, %struct.ScmObj** %stackaddr$makeclosure56824, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49924, %struct.ScmObj* %vs48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49924, %struct.ScmObj* %f48134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49924, %struct.ScmObj* %acc48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49924, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49924, %struct.ScmObj* %_37foldl48130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49924, %struct.ScmObj* %lsts_4348139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49924, %struct.ScmObj* %k48410, i64 6)
%ae49925 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56826 = alloca %struct.ScmObj*, align 8
%fptrToInt56827 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49926 to i64
%ae49926 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56827)
store volatile %struct.ScmObj* %ae49926, %struct.ScmObj** %stackaddr$makeclosure56826, align 8
%argslist55280$ae499240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56828 = alloca %struct.ScmObj*, align 8
%argslist55280$ae499241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49926, %struct.ScmObj* %argslist55280$ae499240)
store volatile %struct.ScmObj* %argslist55280$ae499241, %struct.ScmObj** %stackaddr$prim56828, align 8
%stackaddr$prim56829 = alloca %struct.ScmObj*, align 8
%argslist55280$ae499242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49925, %struct.ScmObj* %argslist55280$ae499241)
store volatile %struct.ScmObj* %argslist55280$ae499242, %struct.ScmObj** %stackaddr$prim56829, align 8
%clofunc56830 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49924)
musttail call tailcc void %clofunc56830(%struct.ScmObj* %ae49924, %struct.ScmObj* %argslist55280$ae499242)
ret void
}

define tailcc void @proc_clo$ae49924(%struct.ScmObj* %env$ae49924,%struct.ScmObj* %current_45args55265) {
%stackaddr$env-ref56831 = alloca %struct.ScmObj*, align 8
%vs48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49924, i64 0)
store %struct.ScmObj* %vs48137, %struct.ScmObj** %stackaddr$env-ref56831
%stackaddr$env-ref56832 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49924, i64 1)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref56832
%stackaddr$env-ref56833 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49924, i64 2)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref56833
%stackaddr$env-ref56834 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49924, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56834
%stackaddr$env-ref56835 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49924, i64 4)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref56835
%stackaddr$env-ref56836 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49924, i64 5)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref56836
%stackaddr$env-ref56837 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49924, i64 6)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref56837
%stackaddr$prim56838 = alloca %struct.ScmObj*, align 8
%_95k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55265)
store volatile %struct.ScmObj* %_95k48417, %struct.ScmObj** %stackaddr$prim56838, align 8
%stackaddr$prim56839 = alloca %struct.ScmObj*, align 8
%current_45args55266 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55265)
store volatile %struct.ScmObj* %current_45args55266, %struct.ScmObj** %stackaddr$prim56839, align 8
%stackaddr$prim56840 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55266)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim56840, align 8
%ae49947 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56841 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48133, %struct.ScmObj* %ae49947)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim56841, align 8
%stackaddr$makeclosure56842 = alloca %struct.ScmObj*, align 8
%fptrToInt56843 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49949 to i64
%ae49949 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56843)
store volatile %struct.ScmObj* %ae49949, %struct.ScmObj** %stackaddr$makeclosure56842, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49949, %struct.ScmObj* %f48134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49949, %struct.ScmObj* %_37foldl48130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49949, %struct.ScmObj* %lsts_4348139, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49949, %struct.ScmObj* %k48410, i64 3)
%argslist55274$_37foldr480520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56844 = alloca %struct.ScmObj*, align 8
%argslist55274$_37foldr480521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48137, %struct.ScmObj* %argslist55274$_37foldr480520)
store volatile %struct.ScmObj* %argslist55274$_37foldr480521, %struct.ScmObj** %stackaddr$prim56844, align 8
%stackaddr$prim56845 = alloca %struct.ScmObj*, align 8
%argslist55274$_37foldr480522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48227, %struct.ScmObj* %argslist55274$_37foldr480521)
store volatile %struct.ScmObj* %argslist55274$_37foldr480522, %struct.ScmObj** %stackaddr$prim56845, align 8
%stackaddr$prim56846 = alloca %struct.ScmObj*, align 8
%argslist55274$_37foldr480523 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48226, %struct.ScmObj* %argslist55274$_37foldr480522)
store volatile %struct.ScmObj* %argslist55274$_37foldr480523, %struct.ScmObj** %stackaddr$prim56846, align 8
%stackaddr$prim56847 = alloca %struct.ScmObj*, align 8
%argslist55274$_37foldr480524 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49949, %struct.ScmObj* %argslist55274$_37foldr480523)
store volatile %struct.ScmObj* %argslist55274$_37foldr480524, %struct.ScmObj** %stackaddr$prim56847, align 8
%clofunc56848 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48052)
musttail call tailcc void %clofunc56848(%struct.ScmObj* %_37foldr48052, %struct.ScmObj* %argslist55274$_37foldr480524)
ret void
}

define tailcc void @proc_clo$ae49949(%struct.ScmObj* %env$ae49949,%struct.ScmObj* %current_45args55268) {
%stackaddr$env-ref56849 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49949, i64 0)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref56849
%stackaddr$env-ref56850 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49949, i64 1)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref56850
%stackaddr$env-ref56851 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49949, i64 2)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref56851
%stackaddr$env-ref56852 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49949, i64 3)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref56852
%stackaddr$prim56853 = alloca %struct.ScmObj*, align 8
%_95k48418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55268)
store volatile %struct.ScmObj* %_95k48418, %struct.ScmObj** %stackaddr$prim56853, align 8
%stackaddr$prim56854 = alloca %struct.ScmObj*, align 8
%current_45args55269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55268)
store volatile %struct.ScmObj* %current_45args55269, %struct.ScmObj** %stackaddr$prim56854, align 8
%stackaddr$prim56855 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55269)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim56855, align 8
%stackaddr$makeclosure56856 = alloca %struct.ScmObj*, align 8
%fptrToInt56857 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49953 to i64
%ae49953 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56857)
store volatile %struct.ScmObj* %ae49953, %struct.ScmObj** %stackaddr$makeclosure56856, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49953, %struct.ScmObj* %f48134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49953, %struct.ScmObj* %_37foldl48130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49953, %struct.ScmObj* %lsts_4348139, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49953, %struct.ScmObj* %k48410, i64 3)
%stackaddr$prim56858 = alloca %struct.ScmObj*, align 8
%cpsargs48421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49953, %struct.ScmObj* %anf_45bind48228)
store volatile %struct.ScmObj* %cpsargs48421, %struct.ScmObj** %stackaddr$prim56858, align 8
%clofunc56859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48134)
musttail call tailcc void %clofunc56859(%struct.ScmObj* %f48134, %struct.ScmObj* %cpsargs48421)
ret void
}

define tailcc void @proc_clo$ae49953(%struct.ScmObj* %env$ae49953,%struct.ScmObj* %current_45args55271) {
%stackaddr$env-ref56860 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49953, i64 0)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref56860
%stackaddr$env-ref56861 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49953, i64 1)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref56861
%stackaddr$env-ref56862 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49953, i64 2)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref56862
%stackaddr$env-ref56863 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49953, i64 3)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref56863
%stackaddr$prim56864 = alloca %struct.ScmObj*, align 8
%_95k48419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55271)
store volatile %struct.ScmObj* %_95k48419, %struct.ScmObj** %stackaddr$prim56864, align 8
%stackaddr$prim56865 = alloca %struct.ScmObj*, align 8
%current_45args55272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55271)
store volatile %struct.ScmObj* %current_45args55272, %struct.ScmObj** %stackaddr$prim56865, align 8
%stackaddr$prim56866 = alloca %struct.ScmObj*, align 8
%acc_4348141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55272)
store volatile %struct.ScmObj* %acc_4348141, %struct.ScmObj** %stackaddr$prim56866, align 8
%stackaddr$prim56867 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348141, %struct.ScmObj* %lsts_4348139)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim56867, align 8
%stackaddr$prim56868 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48134, %struct.ScmObj* %anf_45bind48229)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim56868, align 8
%stackaddr$prim56869 = alloca %struct.ScmObj*, align 8
%cpsargs48420 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48410, %struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %cpsargs48420, %struct.ScmObj** %stackaddr$prim56869, align 8
%clofunc56870 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48130)
musttail call tailcc void %clofunc56870(%struct.ScmObj* %_37foldl48130, %struct.ScmObj* %cpsargs48420)
ret void
}

define tailcc void @proc_clo$ae49926(%struct.ScmObj* %env$ae49926,%struct.ScmObj* %current_45args55275) {
%stackaddr$prim56871 = alloca %struct.ScmObj*, align 8
%k48422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55275)
store volatile %struct.ScmObj* %k48422, %struct.ScmObj** %stackaddr$prim56871, align 8
%stackaddr$prim56872 = alloca %struct.ScmObj*, align 8
%current_45args55276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55275)
store volatile %struct.ScmObj* %current_45args55276, %struct.ScmObj** %stackaddr$prim56872, align 8
%stackaddr$prim56873 = alloca %struct.ScmObj*, align 8
%a48143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55276)
store volatile %struct.ScmObj* %a48143, %struct.ScmObj** %stackaddr$prim56873, align 8
%stackaddr$prim56874 = alloca %struct.ScmObj*, align 8
%current_45args55277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55276)
store volatile %struct.ScmObj* %current_45args55277, %struct.ScmObj** %stackaddr$prim56874, align 8
%stackaddr$prim56875 = alloca %struct.ScmObj*, align 8
%b48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55277)
store volatile %struct.ScmObj* %b48142, %struct.ScmObj** %stackaddr$prim56875, align 8
%stackaddr$prim56876 = alloca %struct.ScmObj*, align 8
%cpsprim48423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48143, %struct.ScmObj* %b48142)
store volatile %struct.ScmObj* %cpsprim48423, %struct.ScmObj** %stackaddr$prim56876, align 8
%ae49930 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55279$k484220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56877 = alloca %struct.ScmObj*, align 8
%argslist55279$k484221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48423, %struct.ScmObj* %argslist55279$k484220)
store volatile %struct.ScmObj* %argslist55279$k484221, %struct.ScmObj** %stackaddr$prim56877, align 8
%stackaddr$prim56878 = alloca %struct.ScmObj*, align 8
%argslist55279$k484222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49930, %struct.ScmObj* %argslist55279$k484221)
store volatile %struct.ScmObj* %argslist55279$k484222, %struct.ScmObj** %stackaddr$prim56878, align 8
%clofunc56879 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48422)
musttail call tailcc void %clofunc56879(%struct.ScmObj* %k48422, %struct.ScmObj* %argslist55279$k484222)
ret void
}

define tailcc void @proc_clo$ae49902(%struct.ScmObj* %env$ae49902,%struct.ScmObj* %current_45args55282) {
%stackaddr$prim56880 = alloca %struct.ScmObj*, align 8
%k48424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55282)
store volatile %struct.ScmObj* %k48424, %struct.ScmObj** %stackaddr$prim56880, align 8
%stackaddr$prim56881 = alloca %struct.ScmObj*, align 8
%current_45args55283 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55282)
store volatile %struct.ScmObj* %current_45args55283, %struct.ScmObj** %stackaddr$prim56881, align 8
%stackaddr$prim56882 = alloca %struct.ScmObj*, align 8
%x48138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55283)
store volatile %struct.ScmObj* %x48138, %struct.ScmObj** %stackaddr$prim56882, align 8
%stackaddr$prim56883 = alloca %struct.ScmObj*, align 8
%cpsprim48425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48138)
store volatile %struct.ScmObj* %cpsprim48425, %struct.ScmObj** %stackaddr$prim56883, align 8
%ae49905 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55285$k484240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56884 = alloca %struct.ScmObj*, align 8
%argslist55285$k484241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48425, %struct.ScmObj* %argslist55285$k484240)
store volatile %struct.ScmObj* %argslist55285$k484241, %struct.ScmObj** %stackaddr$prim56884, align 8
%stackaddr$prim56885 = alloca %struct.ScmObj*, align 8
%argslist55285$k484242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49905, %struct.ScmObj* %argslist55285$k484241)
store volatile %struct.ScmObj* %argslist55285$k484242, %struct.ScmObj** %stackaddr$prim56885, align 8
%clofunc56886 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48424)
musttail call tailcc void %clofunc56886(%struct.ScmObj* %k48424, %struct.ScmObj* %argslist55285$k484242)
ret void
}

define tailcc void @proc_clo$ae49878(%struct.ScmObj* %env$ae49878,%struct.ScmObj* %current_45args55288) {
%stackaddr$prim56887 = alloca %struct.ScmObj*, align 8
%k48426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55288)
store volatile %struct.ScmObj* %k48426, %struct.ScmObj** %stackaddr$prim56887, align 8
%stackaddr$prim56888 = alloca %struct.ScmObj*, align 8
%current_45args55289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55288)
store volatile %struct.ScmObj* %current_45args55289, %struct.ScmObj** %stackaddr$prim56888, align 8
%stackaddr$prim56889 = alloca %struct.ScmObj*, align 8
%x48140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55289)
store volatile %struct.ScmObj* %x48140, %struct.ScmObj** %stackaddr$prim56889, align 8
%stackaddr$prim56890 = alloca %struct.ScmObj*, align 8
%cpsprim48427 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48140)
store volatile %struct.ScmObj* %cpsprim48427, %struct.ScmObj** %stackaddr$prim56890, align 8
%ae49881 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55291$k484260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56891 = alloca %struct.ScmObj*, align 8
%argslist55291$k484261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48427, %struct.ScmObj* %argslist55291$k484260)
store volatile %struct.ScmObj* %argslist55291$k484261, %struct.ScmObj** %stackaddr$prim56891, align 8
%stackaddr$prim56892 = alloca %struct.ScmObj*, align 8
%argslist55291$k484262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49881, %struct.ScmObj* %argslist55291$k484261)
store volatile %struct.ScmObj* %argslist55291$k484262, %struct.ScmObj** %stackaddr$prim56892, align 8
%clofunc56893 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48426)
musttail call tailcc void %clofunc56893(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist55291$k484262)
ret void
}

define tailcc void @proc_clo$ae49830(%struct.ScmObj* %env$ae49830,%struct.ScmObj* %current_45args55294) {
%stackaddr$prim56894 = alloca %struct.ScmObj*, align 8
%k48428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55294)
store volatile %struct.ScmObj* %k48428, %struct.ScmObj** %stackaddr$prim56894, align 8
%stackaddr$prim56895 = alloca %struct.ScmObj*, align 8
%current_45args55295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55294)
store volatile %struct.ScmObj* %current_45args55295, %struct.ScmObj** %stackaddr$prim56895, align 8
%stackaddr$prim56896 = alloca %struct.ScmObj*, align 8
%lst48136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55295)
store volatile %struct.ScmObj* %lst48136, %struct.ScmObj** %stackaddr$prim56896, align 8
%stackaddr$prim56897 = alloca %struct.ScmObj*, align 8
%current_45args55296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55295)
store volatile %struct.ScmObj* %current_45args55296, %struct.ScmObj** %stackaddr$prim56897, align 8
%stackaddr$prim56898 = alloca %struct.ScmObj*, align 8
%b48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55296)
store volatile %struct.ScmObj* %b48135, %struct.ScmObj** %stackaddr$prim56898, align 8
%truthy$cmp56899 = call i64 @is_truthy_value(%struct.ScmObj* %b48135)
%cmp$cmp56899 = icmp eq i64 %truthy$cmp56899, 1
br i1 %cmp$cmp56899, label %truebranch$cmp56899, label %falsebranch$cmp56899
truebranch$cmp56899:
%ae49833 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55298$k484280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56900 = alloca %struct.ScmObj*, align 8
%argslist55298$k484281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48135, %struct.ScmObj* %argslist55298$k484280)
store volatile %struct.ScmObj* %argslist55298$k484281, %struct.ScmObj** %stackaddr$prim56900, align 8
%stackaddr$prim56901 = alloca %struct.ScmObj*, align 8
%argslist55298$k484282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49833, %struct.ScmObj* %argslist55298$k484281)
store volatile %struct.ScmObj* %argslist55298$k484282, %struct.ScmObj** %stackaddr$prim56901, align 8
%clofunc56902 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48428)
musttail call tailcc void %clofunc56902(%struct.ScmObj* %k48428, %struct.ScmObj* %argslist55298$k484282)
ret void
falsebranch$cmp56899:
%stackaddr$prim56903 = alloca %struct.ScmObj*, align 8
%cpsprim48429 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48136)
store volatile %struct.ScmObj* %cpsprim48429, %struct.ScmObj** %stackaddr$prim56903, align 8
%ae49840 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55299$k484280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56904 = alloca %struct.ScmObj*, align 8
%argslist55299$k484281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48429, %struct.ScmObj* %argslist55299$k484280)
store volatile %struct.ScmObj* %argslist55299$k484281, %struct.ScmObj** %stackaddr$prim56904, align 8
%stackaddr$prim56905 = alloca %struct.ScmObj*, align 8
%argslist55299$k484282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49840, %struct.ScmObj* %argslist55299$k484281)
store volatile %struct.ScmObj* %argslist55299$k484282, %struct.ScmObj** %stackaddr$prim56905, align 8
%clofunc56906 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48428)
musttail call tailcc void %clofunc56906(%struct.ScmObj* %k48428, %struct.ScmObj* %argslist55299$k484282)
ret void
}

define tailcc void @proc_clo$ae49671(%struct.ScmObj* %env$ae49671,%struct.ScmObj* %args4807448430) {
%stackaddr$env-ref56907 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49671, i64 0)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref56907
%stackaddr$env-ref56908 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49671, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56908
%stackaddr$env-ref56909 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49671, i64 2)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref56909
%stackaddr$prim56910 = alloca %struct.ScmObj*, align 8
%k48431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4807448430)
store volatile %struct.ScmObj* %k48431, %struct.ScmObj** %stackaddr$prim56910, align 8
%stackaddr$prim56911 = alloca %struct.ScmObj*, align 8
%args48074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4807448430)
store volatile %struct.ScmObj* %args48074, %struct.ScmObj** %stackaddr$prim56911, align 8
%stackaddr$prim56912 = alloca %struct.ScmObj*, align 8
%f48076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48074)
store volatile %struct.ScmObj* %f48076, %struct.ScmObj** %stackaddr$prim56912, align 8
%stackaddr$prim56913 = alloca %struct.ScmObj*, align 8
%lsts48075 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48074)
store volatile %struct.ScmObj* %lsts48075, %struct.ScmObj** %stackaddr$prim56913, align 8
%stackaddr$makeclosure56914 = alloca %struct.ScmObj*, align 8
%fptrToInt56915 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49676 to i64
%ae49676 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56915)
store volatile %struct.ScmObj* %ae49676, %struct.ScmObj** %stackaddr$makeclosure56914, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49676, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49676, %struct.ScmObj* %k48431, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49676, %struct.ScmObj* %lsts48075, i64 2)
%ae49677 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56916 = alloca %struct.ScmObj*, align 8
%fptrToInt56917 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49678 to i64
%ae49678 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56917)
store volatile %struct.ScmObj* %ae49678, %struct.ScmObj** %stackaddr$makeclosure56916, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49678, %struct.ScmObj* %_37last48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49678, %struct.ScmObj* %_37drop_45right48066, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49678, %struct.ScmObj* %f48076, i64 2)
%argslist55318$ae496760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56918 = alloca %struct.ScmObj*, align 8
%argslist55318$ae496761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49678, %struct.ScmObj* %argslist55318$ae496760)
store volatile %struct.ScmObj* %argslist55318$ae496761, %struct.ScmObj** %stackaddr$prim56918, align 8
%stackaddr$prim56919 = alloca %struct.ScmObj*, align 8
%argslist55318$ae496762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49677, %struct.ScmObj* %argslist55318$ae496761)
store volatile %struct.ScmObj* %argslist55318$ae496762, %struct.ScmObj** %stackaddr$prim56919, align 8
%clofunc56920 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49676)
musttail call tailcc void %clofunc56920(%struct.ScmObj* %ae49676, %struct.ScmObj* %argslist55318$ae496762)
ret void
}

define tailcc void @proc_clo$ae49676(%struct.ScmObj* %env$ae49676,%struct.ScmObj* %current_45args55303) {
%stackaddr$env-ref56921 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49676, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56921
%stackaddr$env-ref56922 = alloca %struct.ScmObj*, align 8
%k48431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49676, i64 1)
store %struct.ScmObj* %k48431, %struct.ScmObj** %stackaddr$env-ref56922
%stackaddr$env-ref56923 = alloca %struct.ScmObj*, align 8
%lsts48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49676, i64 2)
store %struct.ScmObj* %lsts48075, %struct.ScmObj** %stackaddr$env-ref56923
%stackaddr$prim56924 = alloca %struct.ScmObj*, align 8
%_95k48432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55303)
store volatile %struct.ScmObj* %_95k48432, %struct.ScmObj** %stackaddr$prim56924, align 8
%stackaddr$prim56925 = alloca %struct.ScmObj*, align 8
%current_45args55304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55303)
store volatile %struct.ScmObj* %current_45args55304, %struct.ScmObj** %stackaddr$prim56925, align 8
%stackaddr$prim56926 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55304)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim56926, align 8
%ae49739 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56927 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49739, %struct.ScmObj* %lsts48075)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim56927, align 8
%stackaddr$prim56928 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48217, %struct.ScmObj* %anf_45bind48218)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim56928, align 8
%stackaddr$prim56929 = alloca %struct.ScmObj*, align 8
%cpsargs48433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48431, %struct.ScmObj* %anf_45bind48219)
store volatile %struct.ScmObj* %cpsargs48433, %struct.ScmObj** %stackaddr$prim56929, align 8
%clofunc56930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48052)
musttail call tailcc void %clofunc56930(%struct.ScmObj* %_37foldr48052, %struct.ScmObj* %cpsargs48433)
ret void
}

define tailcc void @proc_clo$ae49678(%struct.ScmObj* %env$ae49678,%struct.ScmObj* %fargs4807748434) {
%stackaddr$env-ref56931 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49678, i64 0)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref56931
%stackaddr$env-ref56932 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49678, i64 1)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref56932
%stackaddr$env-ref56933 = alloca %struct.ScmObj*, align 8
%f48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49678, i64 2)
store %struct.ScmObj* %f48076, %struct.ScmObj** %stackaddr$env-ref56933
%stackaddr$prim56934 = alloca %struct.ScmObj*, align 8
%k48435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4807748434)
store volatile %struct.ScmObj* %k48435, %struct.ScmObj** %stackaddr$prim56934, align 8
%stackaddr$prim56935 = alloca %struct.ScmObj*, align 8
%fargs48077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4807748434)
store volatile %struct.ScmObj* %fargs48077, %struct.ScmObj** %stackaddr$prim56935, align 8
%stackaddr$makeclosure56936 = alloca %struct.ScmObj*, align 8
%fptrToInt56937 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49682 to i64
%ae49682 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56937)
store volatile %struct.ScmObj* %ae49682, %struct.ScmObj** %stackaddr$makeclosure56936, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49682, %struct.ScmObj* %_37last48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49682, %struct.ScmObj* %k48435, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49682, %struct.ScmObj* %fargs48077, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49682, %struct.ScmObj* %f48076, i64 3)
%ae49684 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist55317$_37drop_45right480660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56938 = alloca %struct.ScmObj*, align 8
%argslist55317$_37drop_45right480661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49684, %struct.ScmObj* %argslist55317$_37drop_45right480660)
store volatile %struct.ScmObj* %argslist55317$_37drop_45right480661, %struct.ScmObj** %stackaddr$prim56938, align 8
%stackaddr$prim56939 = alloca %struct.ScmObj*, align 8
%argslist55317$_37drop_45right480662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48077, %struct.ScmObj* %argslist55317$_37drop_45right480661)
store volatile %struct.ScmObj* %argslist55317$_37drop_45right480662, %struct.ScmObj** %stackaddr$prim56939, align 8
%stackaddr$prim56940 = alloca %struct.ScmObj*, align 8
%argslist55317$_37drop_45right480663 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49682, %struct.ScmObj* %argslist55317$_37drop_45right480662)
store volatile %struct.ScmObj* %argslist55317$_37drop_45right480663, %struct.ScmObj** %stackaddr$prim56940, align 8
%clofunc56941 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48066)
musttail call tailcc void %clofunc56941(%struct.ScmObj* %_37drop_45right48066, %struct.ScmObj* %argslist55317$_37drop_45right480663)
ret void
}

define tailcc void @proc_clo$ae49682(%struct.ScmObj* %env$ae49682,%struct.ScmObj* %current_45args55306) {
%stackaddr$env-ref56942 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49682, i64 0)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref56942
%stackaddr$env-ref56943 = alloca %struct.ScmObj*, align 8
%k48435 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49682, i64 1)
store %struct.ScmObj* %k48435, %struct.ScmObj** %stackaddr$env-ref56943
%stackaddr$env-ref56944 = alloca %struct.ScmObj*, align 8
%fargs48077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49682, i64 2)
store %struct.ScmObj* %fargs48077, %struct.ScmObj** %stackaddr$env-ref56944
%stackaddr$env-ref56945 = alloca %struct.ScmObj*, align 8
%f48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49682, i64 3)
store %struct.ScmObj* %f48076, %struct.ScmObj** %stackaddr$env-ref56945
%stackaddr$prim56946 = alloca %struct.ScmObj*, align 8
%_95k48436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55306)
store volatile %struct.ScmObj* %_95k48436, %struct.ScmObj** %stackaddr$prim56946, align 8
%stackaddr$prim56947 = alloca %struct.ScmObj*, align 8
%current_45args55307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55306)
store volatile %struct.ScmObj* %current_45args55307, %struct.ScmObj** %stackaddr$prim56947, align 8
%stackaddr$prim56948 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55307)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim56948, align 8
%stackaddr$makeclosure56949 = alloca %struct.ScmObj*, align 8
%fptrToInt56950 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49689 to i64
%ae49689 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56950)
store volatile %struct.ScmObj* %ae49689, %struct.ScmObj** %stackaddr$makeclosure56949, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49689, %struct.ScmObj* %_37last48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49689, %struct.ScmObj* %k48435, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49689, %struct.ScmObj* %fargs48077, i64 2)
%stackaddr$prim56951 = alloca %struct.ScmObj*, align 8
%cpsargs48440 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49689, %struct.ScmObj* %anf_45bind48214)
store volatile %struct.ScmObj* %cpsargs48440, %struct.ScmObj** %stackaddr$prim56951, align 8
%clofunc56952 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48076)
musttail call tailcc void %clofunc56952(%struct.ScmObj* %f48076, %struct.ScmObj* %cpsargs48440)
ret void
}

define tailcc void @proc_clo$ae49689(%struct.ScmObj* %env$ae49689,%struct.ScmObj* %current_45args55309) {
%stackaddr$env-ref56953 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49689, i64 0)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref56953
%stackaddr$env-ref56954 = alloca %struct.ScmObj*, align 8
%k48435 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49689, i64 1)
store %struct.ScmObj* %k48435, %struct.ScmObj** %stackaddr$env-ref56954
%stackaddr$env-ref56955 = alloca %struct.ScmObj*, align 8
%fargs48077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49689, i64 2)
store %struct.ScmObj* %fargs48077, %struct.ScmObj** %stackaddr$env-ref56955
%stackaddr$prim56956 = alloca %struct.ScmObj*, align 8
%_95k48437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55309)
store volatile %struct.ScmObj* %_95k48437, %struct.ScmObj** %stackaddr$prim56956, align 8
%stackaddr$prim56957 = alloca %struct.ScmObj*, align 8
%current_45args55310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55309)
store volatile %struct.ScmObj* %current_45args55310, %struct.ScmObj** %stackaddr$prim56957, align 8
%stackaddr$prim56958 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55310)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim56958, align 8
%stackaddr$makeclosure56959 = alloca %struct.ScmObj*, align 8
%fptrToInt56960 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49694 to i64
%ae49694 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56960)
store volatile %struct.ScmObj* %ae49694, %struct.ScmObj** %stackaddr$makeclosure56959, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49694, %struct.ScmObj* %anf_45bind48215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49694, %struct.ScmObj* %k48435, i64 1)
%argslist55316$_37last480690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56961 = alloca %struct.ScmObj*, align 8
%argslist55316$_37last480691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48077, %struct.ScmObj* %argslist55316$_37last480690)
store volatile %struct.ScmObj* %argslist55316$_37last480691, %struct.ScmObj** %stackaddr$prim56961, align 8
%stackaddr$prim56962 = alloca %struct.ScmObj*, align 8
%argslist55316$_37last480692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49694, %struct.ScmObj* %argslist55316$_37last480691)
store volatile %struct.ScmObj* %argslist55316$_37last480692, %struct.ScmObj** %stackaddr$prim56962, align 8
%clofunc56963 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48069)
musttail call tailcc void %clofunc56963(%struct.ScmObj* %_37last48069, %struct.ScmObj* %argslist55316$_37last480692)
ret void
}

define tailcc void @proc_clo$ae49694(%struct.ScmObj* %env$ae49694,%struct.ScmObj* %current_45args55312) {
%stackaddr$env-ref56964 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49694, i64 0)
store %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$env-ref56964
%stackaddr$env-ref56965 = alloca %struct.ScmObj*, align 8
%k48435 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49694, i64 1)
store %struct.ScmObj* %k48435, %struct.ScmObj** %stackaddr$env-ref56965
%stackaddr$prim56966 = alloca %struct.ScmObj*, align 8
%_95k48438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55312)
store volatile %struct.ScmObj* %_95k48438, %struct.ScmObj** %stackaddr$prim56966, align 8
%stackaddr$prim56967 = alloca %struct.ScmObj*, align 8
%current_45args55313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55312)
store volatile %struct.ScmObj* %current_45args55313, %struct.ScmObj** %stackaddr$prim56967, align 8
%stackaddr$prim56968 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55313)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim56968, align 8
%stackaddr$prim56969 = alloca %struct.ScmObj*, align 8
%cpsprim48439 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48215, %struct.ScmObj* %anf_45bind48216)
store volatile %struct.ScmObj* %cpsprim48439, %struct.ScmObj** %stackaddr$prim56969, align 8
%ae49699 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55315$k484350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56970 = alloca %struct.ScmObj*, align 8
%argslist55315$k484351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48439, %struct.ScmObj* %argslist55315$k484350)
store volatile %struct.ScmObj* %argslist55315$k484351, %struct.ScmObj** %stackaddr$prim56970, align 8
%stackaddr$prim56971 = alloca %struct.ScmObj*, align 8
%argslist55315$k484352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49699, %struct.ScmObj* %argslist55315$k484351)
store volatile %struct.ScmObj* %argslist55315$k484352, %struct.ScmObj** %stackaddr$prim56971, align 8
%clofunc56972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48435)
musttail call tailcc void %clofunc56972(%struct.ScmObj* %k48435, %struct.ScmObj* %argslist55315$k484352)
ret void
}

define tailcc void @proc_clo$ae49594(%struct.ScmObj* %env$ae49594,%struct.ScmObj* %current_45args55320) {
%stackaddr$env-ref56973 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49594, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56973
%stackaddr$prim56974 = alloca %struct.ScmObj*, align 8
%k48441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55320)
store volatile %struct.ScmObj* %k48441, %struct.ScmObj** %stackaddr$prim56974, align 8
%stackaddr$prim56975 = alloca %struct.ScmObj*, align 8
%current_45args55321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55320)
store volatile %struct.ScmObj* %current_45args55321, %struct.ScmObj** %stackaddr$prim56975, align 8
%stackaddr$prim56976 = alloca %struct.ScmObj*, align 8
%f48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55321)
store volatile %struct.ScmObj* %f48080, %struct.ScmObj** %stackaddr$prim56976, align 8
%stackaddr$prim56977 = alloca %struct.ScmObj*, align 8
%current_45args55322 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55321)
store volatile %struct.ScmObj* %current_45args55322, %struct.ScmObj** %stackaddr$prim56977, align 8
%stackaddr$prim56978 = alloca %struct.ScmObj*, align 8
%lst48079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55322)
store volatile %struct.ScmObj* %lst48079, %struct.ScmObj** %stackaddr$prim56978, align 8
%stackaddr$makeclosure56979 = alloca %struct.ScmObj*, align 8
%fptrToInt56980 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49595 to i64
%ae49595 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56980)
store volatile %struct.ScmObj* %ae49595, %struct.ScmObj** %stackaddr$makeclosure56979, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49595, %struct.ScmObj* %lst48079, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49595, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49595, %struct.ScmObj* %k48441, i64 2)
%ae49596 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56981 = alloca %struct.ScmObj*, align 8
%fptrToInt56982 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49597 to i64
%ae49597 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56982)
store volatile %struct.ScmObj* %ae49597, %struct.ScmObj** %stackaddr$makeclosure56981, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49597, %struct.ScmObj* %f48080, i64 0)
%argslist55337$ae495950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56983 = alloca %struct.ScmObj*, align 8
%argslist55337$ae495951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49597, %struct.ScmObj* %argslist55337$ae495950)
store volatile %struct.ScmObj* %argslist55337$ae495951, %struct.ScmObj** %stackaddr$prim56983, align 8
%stackaddr$prim56984 = alloca %struct.ScmObj*, align 8
%argslist55337$ae495952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49596, %struct.ScmObj* %argslist55337$ae495951)
store volatile %struct.ScmObj* %argslist55337$ae495952, %struct.ScmObj** %stackaddr$prim56984, align 8
%clofunc56985 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49595)
musttail call tailcc void %clofunc56985(%struct.ScmObj* %ae49595, %struct.ScmObj* %argslist55337$ae495952)
ret void
}

define tailcc void @proc_clo$ae49595(%struct.ScmObj* %env$ae49595,%struct.ScmObj* %current_45args55324) {
%stackaddr$env-ref56986 = alloca %struct.ScmObj*, align 8
%lst48079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49595, i64 0)
store %struct.ScmObj* %lst48079, %struct.ScmObj** %stackaddr$env-ref56986
%stackaddr$env-ref56987 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49595, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56987
%stackaddr$env-ref56988 = alloca %struct.ScmObj*, align 8
%k48441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49595, i64 2)
store %struct.ScmObj* %k48441, %struct.ScmObj** %stackaddr$env-ref56988
%stackaddr$prim56989 = alloca %struct.ScmObj*, align 8
%_95k48442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55324)
store volatile %struct.ScmObj* %_95k48442, %struct.ScmObj** %stackaddr$prim56989, align 8
%stackaddr$prim56990 = alloca %struct.ScmObj*, align 8
%current_45args55325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55324)
store volatile %struct.ScmObj* %current_45args55325, %struct.ScmObj** %stackaddr$prim56990, align 8
%stackaddr$prim56991 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55325)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim56991, align 8
%ae49629 = call %struct.ScmObj* @const_init_null()
%argslist55327$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56992 = alloca %struct.ScmObj*, align 8
%argslist55327$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48079, %struct.ScmObj* %argslist55327$_37foldr1480470)
store volatile %struct.ScmObj* %argslist55327$_37foldr1480471, %struct.ScmObj** %stackaddr$prim56992, align 8
%stackaddr$prim56993 = alloca %struct.ScmObj*, align 8
%argslist55327$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49629, %struct.ScmObj* %argslist55327$_37foldr1480471)
store volatile %struct.ScmObj* %argslist55327$_37foldr1480472, %struct.ScmObj** %stackaddr$prim56993, align 8
%stackaddr$prim56994 = alloca %struct.ScmObj*, align 8
%argslist55327$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48213, %struct.ScmObj* %argslist55327$_37foldr1480472)
store volatile %struct.ScmObj* %argslist55327$_37foldr1480473, %struct.ScmObj** %stackaddr$prim56994, align 8
%stackaddr$prim56995 = alloca %struct.ScmObj*, align 8
%argslist55327$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48441, %struct.ScmObj* %argslist55327$_37foldr1480473)
store volatile %struct.ScmObj* %argslist55327$_37foldr1480474, %struct.ScmObj** %stackaddr$prim56995, align 8
%clofunc56996 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc56996(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist55327$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae49597(%struct.ScmObj* %env$ae49597,%struct.ScmObj* %current_45args55328) {
%stackaddr$env-ref56997 = alloca %struct.ScmObj*, align 8
%f48080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49597, i64 0)
store %struct.ScmObj* %f48080, %struct.ScmObj** %stackaddr$env-ref56997
%stackaddr$prim56998 = alloca %struct.ScmObj*, align 8
%k48443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55328)
store volatile %struct.ScmObj* %k48443, %struct.ScmObj** %stackaddr$prim56998, align 8
%stackaddr$prim56999 = alloca %struct.ScmObj*, align 8
%current_45args55329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55328)
store volatile %struct.ScmObj* %current_45args55329, %struct.ScmObj** %stackaddr$prim56999, align 8
%stackaddr$prim57000 = alloca %struct.ScmObj*, align 8
%v48082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55329)
store volatile %struct.ScmObj* %v48082, %struct.ScmObj** %stackaddr$prim57000, align 8
%stackaddr$prim57001 = alloca %struct.ScmObj*, align 8
%current_45args55330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55329)
store volatile %struct.ScmObj* %current_45args55330, %struct.ScmObj** %stackaddr$prim57001, align 8
%stackaddr$prim57002 = alloca %struct.ScmObj*, align 8
%r48081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55330)
store volatile %struct.ScmObj* %r48081, %struct.ScmObj** %stackaddr$prim57002, align 8
%stackaddr$makeclosure57003 = alloca %struct.ScmObj*, align 8
%fptrToInt57004 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49599 to i64
%ae49599 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57004)
store volatile %struct.ScmObj* %ae49599, %struct.ScmObj** %stackaddr$makeclosure57003, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49599, %struct.ScmObj* %r48081, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49599, %struct.ScmObj* %k48443, i64 1)
%argslist55336$f480800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57005 = alloca %struct.ScmObj*, align 8
%argslist55336$f480801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48082, %struct.ScmObj* %argslist55336$f480800)
store volatile %struct.ScmObj* %argslist55336$f480801, %struct.ScmObj** %stackaddr$prim57005, align 8
%stackaddr$prim57006 = alloca %struct.ScmObj*, align 8
%argslist55336$f480802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49599, %struct.ScmObj* %argslist55336$f480801)
store volatile %struct.ScmObj* %argslist55336$f480802, %struct.ScmObj** %stackaddr$prim57006, align 8
%clofunc57007 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48080)
musttail call tailcc void %clofunc57007(%struct.ScmObj* %f48080, %struct.ScmObj* %argslist55336$f480802)
ret void
}

define tailcc void @proc_clo$ae49599(%struct.ScmObj* %env$ae49599,%struct.ScmObj* %current_45args55332) {
%stackaddr$env-ref57008 = alloca %struct.ScmObj*, align 8
%r48081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49599, i64 0)
store %struct.ScmObj* %r48081, %struct.ScmObj** %stackaddr$env-ref57008
%stackaddr$env-ref57009 = alloca %struct.ScmObj*, align 8
%k48443 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49599, i64 1)
store %struct.ScmObj* %k48443, %struct.ScmObj** %stackaddr$env-ref57009
%stackaddr$prim57010 = alloca %struct.ScmObj*, align 8
%_95k48444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55332)
store volatile %struct.ScmObj* %_95k48444, %struct.ScmObj** %stackaddr$prim57010, align 8
%stackaddr$prim57011 = alloca %struct.ScmObj*, align 8
%current_45args55333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55332)
store volatile %struct.ScmObj* %current_45args55333, %struct.ScmObj** %stackaddr$prim57011, align 8
%stackaddr$prim57012 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55333)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim57012, align 8
%stackaddr$prim57013 = alloca %struct.ScmObj*, align 8
%cpsprim48445 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48212, %struct.ScmObj* %r48081)
store volatile %struct.ScmObj* %cpsprim48445, %struct.ScmObj** %stackaddr$prim57013, align 8
%ae49604 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55335$k484430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57014 = alloca %struct.ScmObj*, align 8
%argslist55335$k484431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48445, %struct.ScmObj* %argslist55335$k484430)
store volatile %struct.ScmObj* %argslist55335$k484431, %struct.ScmObj** %stackaddr$prim57014, align 8
%stackaddr$prim57015 = alloca %struct.ScmObj*, align 8
%argslist55335$k484432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49604, %struct.ScmObj* %argslist55335$k484431)
store volatile %struct.ScmObj* %argslist55335$k484432, %struct.ScmObj** %stackaddr$prim57015, align 8
%clofunc57016 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48443)
musttail call tailcc void %clofunc57016(%struct.ScmObj* %k48443, %struct.ScmObj* %argslist55335$k484432)
ret void
}

define tailcc void @proc_clo$ae49208(%struct.ScmObj* %env$ae49208,%struct.ScmObj* %current_45args55340) {
%stackaddr$env-ref57017 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49208, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref57017
%stackaddr$env-ref57018 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49208, i64 1)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref57018
%stackaddr$prim57019 = alloca %struct.ScmObj*, align 8
%k48446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55340)
store volatile %struct.ScmObj* %k48446, %struct.ScmObj** %stackaddr$prim57019, align 8
%stackaddr$prim57020 = alloca %struct.ScmObj*, align 8
%current_45args55341 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55340)
store volatile %struct.ScmObj* %current_45args55341, %struct.ScmObj** %stackaddr$prim57020, align 8
%stackaddr$prim57021 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55341)
store volatile %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$prim57021, align 8
%ae49210 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57022 = alloca %struct.ScmObj*, align 8
%fptrToInt57023 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49211 to i64
%ae49211 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57023)
store volatile %struct.ScmObj* %ae49211, %struct.ScmObj** %stackaddr$makeclosure57022, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %_37foldr48053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %_37map148043, i64 2)
%argslist55398$k484460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57024 = alloca %struct.ScmObj*, align 8
%argslist55398$k484461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49211, %struct.ScmObj* %argslist55398$k484460)
store volatile %struct.ScmObj* %argslist55398$k484461, %struct.ScmObj** %stackaddr$prim57024, align 8
%stackaddr$prim57025 = alloca %struct.ScmObj*, align 8
%argslist55398$k484462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49210, %struct.ScmObj* %argslist55398$k484461)
store volatile %struct.ScmObj* %argslist55398$k484462, %struct.ScmObj** %stackaddr$prim57025, align 8
%clofunc57026 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48446)
musttail call tailcc void %clofunc57026(%struct.ScmObj* %k48446, %struct.ScmObj* %argslist55398$k484462)
ret void
}

define tailcc void @proc_clo$ae49211(%struct.ScmObj* %env$ae49211,%struct.ScmObj* %args4805448447) {
%stackaddr$env-ref57027 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 0)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref57027
%stackaddr$env-ref57028 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref57028
%stackaddr$env-ref57029 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 2)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref57029
%stackaddr$prim57030 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4805448447)
store volatile %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$prim57030, align 8
%stackaddr$prim57031 = alloca %struct.ScmObj*, align 8
%args48054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4805448447)
store volatile %struct.ScmObj* %args48054, %struct.ScmObj** %stackaddr$prim57031, align 8
%stackaddr$prim57032 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48054)
store volatile %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$prim57032, align 8
%stackaddr$prim57033 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48054)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim57033, align 8
%stackaddr$prim57034 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48199)
store volatile %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$prim57034, align 8
%stackaddr$prim57035 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48054)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim57035, align 8
%stackaddr$prim57036 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48200)
store volatile %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$prim57036, align 8
%stackaddr$makeclosure57037 = alloca %struct.ScmObj*, align 8
%fptrToInt57038 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49219 to i64
%ae49219 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57038)
store volatile %struct.ScmObj* %ae49219, %struct.ScmObj** %stackaddr$makeclosure57037, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49219, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49219, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49219, %struct.ScmObj* %lsts48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49219, %struct.ScmObj* %_37foldr48053, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49219, %struct.ScmObj* %k48448, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49219, %struct.ScmObj* %_37foldr148047, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49219, %struct.ScmObj* %_37map148043, i64 6)
%ae49220 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57039 = alloca %struct.ScmObj*, align 8
%fptrToInt57040 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49221 to i64
%ae49221 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57040)
store volatile %struct.ScmObj* %ae49221, %struct.ScmObj** %stackaddr$makeclosure57039, align 8
%argslist55397$ae492190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57041 = alloca %struct.ScmObj*, align 8
%argslist55397$ae492191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49221, %struct.ScmObj* %argslist55397$ae492190)
store volatile %struct.ScmObj* %argslist55397$ae492191, %struct.ScmObj** %stackaddr$prim57041, align 8
%stackaddr$prim57042 = alloca %struct.ScmObj*, align 8
%argslist55397$ae492192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49220, %struct.ScmObj* %argslist55397$ae492191)
store volatile %struct.ScmObj* %argslist55397$ae492192, %struct.ScmObj** %stackaddr$prim57042, align 8
%clofunc57043 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49219)
musttail call tailcc void %clofunc57043(%struct.ScmObj* %ae49219, %struct.ScmObj* %argslist55397$ae492192)
ret void
}

define tailcc void @proc_clo$ae49219(%struct.ScmObj* %env$ae49219,%struct.ScmObj* %current_45args55343) {
%stackaddr$env-ref57044 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49219, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref57044
%stackaddr$env-ref57045 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49219, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref57045
%stackaddr$env-ref57046 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49219, i64 2)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref57046
%stackaddr$env-ref57047 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49219, i64 3)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref57047
%stackaddr$env-ref57048 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49219, i64 4)
store %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$env-ref57048
%stackaddr$env-ref57049 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49219, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref57049
%stackaddr$env-ref57050 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49219, i64 6)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref57050
%stackaddr$prim57051 = alloca %struct.ScmObj*, align 8
%_95k48449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55343)
store volatile %struct.ScmObj* %_95k48449, %struct.ScmObj** %stackaddr$prim57051, align 8
%stackaddr$prim57052 = alloca %struct.ScmObj*, align 8
%current_45args55344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55343)
store volatile %struct.ScmObj* %current_45args55344, %struct.ScmObj** %stackaddr$prim57052, align 8
%stackaddr$prim57053 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55344)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim57053, align 8
%stackaddr$makeclosure57054 = alloca %struct.ScmObj*, align 8
%fptrToInt57055 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49251 to i64
%ae49251 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57055)
store volatile %struct.ScmObj* %ae49251, %struct.ScmObj** %stackaddr$makeclosure57054, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %lsts48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %_37foldr48053, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %k48448, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %_37foldr148047, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %_37map148043, i64 6)
%ae49253 = call %struct.ScmObj* @const_init_false()
%argslist55390$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57056 = alloca %struct.ScmObj*, align 8
%argslist55390$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48055, %struct.ScmObj* %argslist55390$_37foldr1480470)
store volatile %struct.ScmObj* %argslist55390$_37foldr1480471, %struct.ScmObj** %stackaddr$prim57056, align 8
%stackaddr$prim57057 = alloca %struct.ScmObj*, align 8
%argslist55390$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49253, %struct.ScmObj* %argslist55390$_37foldr1480471)
store volatile %struct.ScmObj* %argslist55390$_37foldr1480472, %struct.ScmObj** %stackaddr$prim57057, align 8
%stackaddr$prim57058 = alloca %struct.ScmObj*, align 8
%argslist55390$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48201, %struct.ScmObj* %argslist55390$_37foldr1480472)
store volatile %struct.ScmObj* %argslist55390$_37foldr1480473, %struct.ScmObj** %stackaddr$prim57058, align 8
%stackaddr$prim57059 = alloca %struct.ScmObj*, align 8
%argslist55390$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49251, %struct.ScmObj* %argslist55390$_37foldr1480473)
store volatile %struct.ScmObj* %argslist55390$_37foldr1480474, %struct.ScmObj** %stackaddr$prim57059, align 8
%clofunc57060 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc57060(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist55390$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae49251(%struct.ScmObj* %env$ae49251,%struct.ScmObj* %current_45args55346) {
%stackaddr$env-ref57061 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref57061
%stackaddr$env-ref57062 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref57062
%stackaddr$env-ref57063 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 2)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref57063
%stackaddr$env-ref57064 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 3)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref57064
%stackaddr$env-ref57065 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 4)
store %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$env-ref57065
%stackaddr$env-ref57066 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref57066
%stackaddr$env-ref57067 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 6)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref57067
%stackaddr$prim57068 = alloca %struct.ScmObj*, align 8
%_95k48450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55346)
store volatile %struct.ScmObj* %_95k48450, %struct.ScmObj** %stackaddr$prim57068, align 8
%stackaddr$prim57069 = alloca %struct.ScmObj*, align 8
%current_45args55347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55346)
store volatile %struct.ScmObj* %current_45args55347, %struct.ScmObj** %stackaddr$prim57069, align 8
%stackaddr$prim57070 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55347)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim57070, align 8
%truthy$cmp57071 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48202)
%cmp$cmp57071 = icmp eq i64 %truthy$cmp57071, 1
br i1 %cmp$cmp57071, label %truebranch$cmp57071, label %falsebranch$cmp57071
truebranch$cmp57071:
%ae49262 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55349$k484480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57072 = alloca %struct.ScmObj*, align 8
%argslist55349$k484481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48056, %struct.ScmObj* %argslist55349$k484480)
store volatile %struct.ScmObj* %argslist55349$k484481, %struct.ScmObj** %stackaddr$prim57072, align 8
%stackaddr$prim57073 = alloca %struct.ScmObj*, align 8
%argslist55349$k484482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49262, %struct.ScmObj* %argslist55349$k484481)
store volatile %struct.ScmObj* %argslist55349$k484482, %struct.ScmObj** %stackaddr$prim57073, align 8
%clofunc57074 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48448)
musttail call tailcc void %clofunc57074(%struct.ScmObj* %k48448, %struct.ScmObj* %argslist55349$k484482)
ret void
falsebranch$cmp57071:
%stackaddr$makeclosure57075 = alloca %struct.ScmObj*, align 8
%fptrToInt57076 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49267 to i64
%ae49267 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57076)
store volatile %struct.ScmObj* %ae49267, %struct.ScmObj** %stackaddr$makeclosure57075, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49267, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49267, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49267, %struct.ScmObj* %lsts48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49267, %struct.ScmObj* %_37foldr48053, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49267, %struct.ScmObj* %k48448, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49267, %struct.ScmObj* %_37foldr148047, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49267, %struct.ScmObj* %_37map148043, i64 6)
%ae49268 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57077 = alloca %struct.ScmObj*, align 8
%fptrToInt57078 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49269 to i64
%ae49269 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57078)
store volatile %struct.ScmObj* %ae49269, %struct.ScmObj** %stackaddr$makeclosure57077, align 8
%argslist55389$ae492670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57079 = alloca %struct.ScmObj*, align 8
%argslist55389$ae492671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49269, %struct.ScmObj* %argslist55389$ae492670)
store volatile %struct.ScmObj* %argslist55389$ae492671, %struct.ScmObj** %stackaddr$prim57079, align 8
%stackaddr$prim57080 = alloca %struct.ScmObj*, align 8
%argslist55389$ae492672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49268, %struct.ScmObj* %argslist55389$ae492671)
store volatile %struct.ScmObj* %argslist55389$ae492672, %struct.ScmObj** %stackaddr$prim57080, align 8
%clofunc57081 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49267)
musttail call tailcc void %clofunc57081(%struct.ScmObj* %ae49267, %struct.ScmObj* %argslist55389$ae492672)
ret void
}

define tailcc void @proc_clo$ae49267(%struct.ScmObj* %env$ae49267,%struct.ScmObj* %current_45args55350) {
%stackaddr$env-ref57082 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49267, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref57082
%stackaddr$env-ref57083 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49267, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref57083
%stackaddr$env-ref57084 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49267, i64 2)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref57084
%stackaddr$env-ref57085 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49267, i64 3)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref57085
%stackaddr$env-ref57086 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49267, i64 4)
store %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$env-ref57086
%stackaddr$env-ref57087 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49267, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref57087
%stackaddr$env-ref57088 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49267, i64 6)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref57088
%stackaddr$prim57089 = alloca %struct.ScmObj*, align 8
%_95k48451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55350)
store volatile %struct.ScmObj* %_95k48451, %struct.ScmObj** %stackaddr$prim57089, align 8
%stackaddr$prim57090 = alloca %struct.ScmObj*, align 8
%current_45args55351 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55350)
store volatile %struct.ScmObj* %current_45args55351, %struct.ScmObj** %stackaddr$prim57090, align 8
%stackaddr$prim57091 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55351)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim57091, align 8
%stackaddr$makeclosure57092 = alloca %struct.ScmObj*, align 8
%fptrToInt57093 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49288 to i64
%ae49288 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57093)
store volatile %struct.ScmObj* %ae49288, %struct.ScmObj** %stackaddr$makeclosure57092, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %lsts48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %_37foldr48053, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %k48448, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %_37foldr148047, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %_37map148043, i64 6)
%argslist55384$_37map1480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57094 = alloca %struct.ScmObj*, align 8
%argslist55384$_37map1480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48055, %struct.ScmObj* %argslist55384$_37map1480430)
store volatile %struct.ScmObj* %argslist55384$_37map1480431, %struct.ScmObj** %stackaddr$prim57094, align 8
%stackaddr$prim57095 = alloca %struct.ScmObj*, align 8
%argslist55384$_37map1480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48203, %struct.ScmObj* %argslist55384$_37map1480431)
store volatile %struct.ScmObj* %argslist55384$_37map1480432, %struct.ScmObj** %stackaddr$prim57095, align 8
%stackaddr$prim57096 = alloca %struct.ScmObj*, align 8
%argslist55384$_37map1480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49288, %struct.ScmObj* %argslist55384$_37map1480432)
store volatile %struct.ScmObj* %argslist55384$_37map1480433, %struct.ScmObj** %stackaddr$prim57096, align 8
%clofunc57097 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148043)
musttail call tailcc void %clofunc57097(%struct.ScmObj* %_37map148043, %struct.ScmObj* %argslist55384$_37map1480433)
ret void
}

define tailcc void @proc_clo$ae49288(%struct.ScmObj* %env$ae49288,%struct.ScmObj* %current_45args55353) {
%stackaddr$env-ref57098 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref57098
%stackaddr$env-ref57099 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref57099
%stackaddr$env-ref57100 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 2)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref57100
%stackaddr$env-ref57101 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 3)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref57101
%stackaddr$env-ref57102 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 4)
store %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$env-ref57102
%stackaddr$env-ref57103 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref57103
%stackaddr$env-ref57104 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 6)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref57104
%stackaddr$prim57105 = alloca %struct.ScmObj*, align 8
%_95k48452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55353)
store volatile %struct.ScmObj* %_95k48452, %struct.ScmObj** %stackaddr$prim57105, align 8
%stackaddr$prim57106 = alloca %struct.ScmObj*, align 8
%current_45args55354 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55353)
store volatile %struct.ScmObj* %current_45args55354, %struct.ScmObj** %stackaddr$prim57106, align 8
%stackaddr$prim57107 = alloca %struct.ScmObj*, align 8
%lsts_4348062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55354)
store volatile %struct.ScmObj* %lsts_4348062, %struct.ScmObj** %stackaddr$prim57107, align 8
%stackaddr$makeclosure57108 = alloca %struct.ScmObj*, align 8
%fptrToInt57109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49291 to i64
%ae49291 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57109)
store volatile %struct.ScmObj* %ae49291, %struct.ScmObj** %stackaddr$makeclosure57108, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49291, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49291, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49291, %struct.ScmObj* %lsts48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49291, %struct.ScmObj* %_37foldr48053, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49291, %struct.ScmObj* %k48448, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49291, %struct.ScmObj* %_37foldr148047, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49291, %struct.ScmObj* %lsts_4348062, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49291, %struct.ScmObj* %_37map148043, i64 7)
%ae49292 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57110 = alloca %struct.ScmObj*, align 8
%fptrToInt57111 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49293 to i64
%ae49293 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57111)
store volatile %struct.ScmObj* %ae49293, %struct.ScmObj** %stackaddr$makeclosure57110, align 8
%argslist55383$ae492910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57112 = alloca %struct.ScmObj*, align 8
%argslist55383$ae492911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49293, %struct.ScmObj* %argslist55383$ae492910)
store volatile %struct.ScmObj* %argslist55383$ae492911, %struct.ScmObj** %stackaddr$prim57112, align 8
%stackaddr$prim57113 = alloca %struct.ScmObj*, align 8
%argslist55383$ae492912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49292, %struct.ScmObj* %argslist55383$ae492911)
store volatile %struct.ScmObj* %argslist55383$ae492912, %struct.ScmObj** %stackaddr$prim57113, align 8
%clofunc57114 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49291)
musttail call tailcc void %clofunc57114(%struct.ScmObj* %ae49291, %struct.ScmObj* %argslist55383$ae492912)
ret void
}

define tailcc void @proc_clo$ae49291(%struct.ScmObj* %env$ae49291,%struct.ScmObj* %current_45args55356) {
%stackaddr$env-ref57115 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49291, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref57115
%stackaddr$env-ref57116 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49291, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref57116
%stackaddr$env-ref57117 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49291, i64 2)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref57117
%stackaddr$env-ref57118 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49291, i64 3)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref57118
%stackaddr$env-ref57119 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49291, i64 4)
store %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$env-ref57119
%stackaddr$env-ref57120 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49291, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref57120
%stackaddr$env-ref57121 = alloca %struct.ScmObj*, align 8
%lsts_4348062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49291, i64 6)
store %struct.ScmObj* %lsts_4348062, %struct.ScmObj** %stackaddr$env-ref57121
%stackaddr$env-ref57122 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49291, i64 7)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref57122
%stackaddr$prim57123 = alloca %struct.ScmObj*, align 8
%_95k48453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55356)
store volatile %struct.ScmObj* %_95k48453, %struct.ScmObj** %stackaddr$prim57123, align 8
%stackaddr$prim57124 = alloca %struct.ScmObj*, align 8
%current_45args55357 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55356)
store volatile %struct.ScmObj* %current_45args55357, %struct.ScmObj** %stackaddr$prim57124, align 8
%stackaddr$prim57125 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55357)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim57125, align 8
%stackaddr$makeclosure57126 = alloca %struct.ScmObj*, align 8
%fptrToInt57127 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49312 to i64
%ae49312 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57127)
store volatile %struct.ScmObj* %ae49312, %struct.ScmObj** %stackaddr$makeclosure57126, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49312, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49312, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49312, %struct.ScmObj* %_37foldr48053, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49312, %struct.ScmObj* %k48448, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49312, %struct.ScmObj* %_37foldr148047, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49312, %struct.ScmObj* %lsts_4348062, i64 5)
%argslist55378$_37map1480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57128 = alloca %struct.ScmObj*, align 8
%argslist55378$_37map1480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48055, %struct.ScmObj* %argslist55378$_37map1480430)
store volatile %struct.ScmObj* %argslist55378$_37map1480431, %struct.ScmObj** %stackaddr$prim57128, align 8
%stackaddr$prim57129 = alloca %struct.ScmObj*, align 8
%argslist55378$_37map1480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48204, %struct.ScmObj* %argslist55378$_37map1480431)
store volatile %struct.ScmObj* %argslist55378$_37map1480432, %struct.ScmObj** %stackaddr$prim57129, align 8
%stackaddr$prim57130 = alloca %struct.ScmObj*, align 8
%argslist55378$_37map1480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49312, %struct.ScmObj* %argslist55378$_37map1480432)
store volatile %struct.ScmObj* %argslist55378$_37map1480433, %struct.ScmObj** %stackaddr$prim57130, align 8
%clofunc57131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148043)
musttail call tailcc void %clofunc57131(%struct.ScmObj* %_37map148043, %struct.ScmObj* %argslist55378$_37map1480433)
ret void
}

define tailcc void @proc_clo$ae49312(%struct.ScmObj* %env$ae49312,%struct.ScmObj* %current_45args55359) {
%stackaddr$env-ref57132 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49312, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref57132
%stackaddr$env-ref57133 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49312, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref57133
%stackaddr$env-ref57134 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49312, i64 2)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref57134
%stackaddr$env-ref57135 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49312, i64 3)
store %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$env-ref57135
%stackaddr$env-ref57136 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49312, i64 4)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref57136
%stackaddr$env-ref57137 = alloca %struct.ScmObj*, align 8
%lsts_4348062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49312, i64 5)
store %struct.ScmObj* %lsts_4348062, %struct.ScmObj** %stackaddr$env-ref57137
%stackaddr$prim57138 = alloca %struct.ScmObj*, align 8
%_95k48454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55359)
store volatile %struct.ScmObj* %_95k48454, %struct.ScmObj** %stackaddr$prim57138, align 8
%stackaddr$prim57139 = alloca %struct.ScmObj*, align 8
%current_45args55360 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55359)
store volatile %struct.ScmObj* %current_45args55360, %struct.ScmObj** %stackaddr$prim57139, align 8
%stackaddr$prim57140 = alloca %struct.ScmObj*, align 8
%vs48060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55360)
store volatile %struct.ScmObj* %vs48060, %struct.ScmObj** %stackaddr$prim57140, align 8
%stackaddr$makeclosure57141 = alloca %struct.ScmObj*, align 8
%fptrToInt57142 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49315 to i64
%ae49315 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57142)
store volatile %struct.ScmObj* %ae49315, %struct.ScmObj** %stackaddr$makeclosure57141, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49315, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49315, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49315, %struct.ScmObj* %_37foldr48053, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49315, %struct.ScmObj* %k48448, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49315, %struct.ScmObj* %_37foldr148047, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49315, %struct.ScmObj* %lsts_4348062, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49315, %struct.ScmObj* %vs48060, i64 6)
%ae49316 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57143 = alloca %struct.ScmObj*, align 8
%fptrToInt57144 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49317 to i64
%ae49317 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57144)
store volatile %struct.ScmObj* %ae49317, %struct.ScmObj** %stackaddr$makeclosure57143, align 8
%argslist55377$ae493150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57145 = alloca %struct.ScmObj*, align 8
%argslist55377$ae493151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49317, %struct.ScmObj* %argslist55377$ae493150)
store volatile %struct.ScmObj* %argslist55377$ae493151, %struct.ScmObj** %stackaddr$prim57145, align 8
%stackaddr$prim57146 = alloca %struct.ScmObj*, align 8
%argslist55377$ae493152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49316, %struct.ScmObj* %argslist55377$ae493151)
store volatile %struct.ScmObj* %argslist55377$ae493152, %struct.ScmObj** %stackaddr$prim57146, align 8
%clofunc57147 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49315)
musttail call tailcc void %clofunc57147(%struct.ScmObj* %ae49315, %struct.ScmObj* %argslist55377$ae493152)
ret void
}

define tailcc void @proc_clo$ae49315(%struct.ScmObj* %env$ae49315,%struct.ScmObj* %current_45args55362) {
%stackaddr$env-ref57148 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49315, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref57148
%stackaddr$env-ref57149 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49315, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref57149
%stackaddr$env-ref57150 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49315, i64 2)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref57150
%stackaddr$env-ref57151 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49315, i64 3)
store %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$env-ref57151
%stackaddr$env-ref57152 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49315, i64 4)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref57152
%stackaddr$env-ref57153 = alloca %struct.ScmObj*, align 8
%lsts_4348062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49315, i64 5)
store %struct.ScmObj* %lsts_4348062, %struct.ScmObj** %stackaddr$env-ref57153
%stackaddr$env-ref57154 = alloca %struct.ScmObj*, align 8
%vs48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49315, i64 6)
store %struct.ScmObj* %vs48060, %struct.ScmObj** %stackaddr$env-ref57154
%stackaddr$prim57155 = alloca %struct.ScmObj*, align 8
%_95k48455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55362)
store volatile %struct.ScmObj* %_95k48455, %struct.ScmObj** %stackaddr$prim57155, align 8
%stackaddr$prim57156 = alloca %struct.ScmObj*, align 8
%current_45args55363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55362)
store volatile %struct.ScmObj* %current_45args55363, %struct.ScmObj** %stackaddr$prim57156, align 8
%stackaddr$prim57157 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55363)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim57157, align 8
%stackaddr$prim57158 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48056, %struct.ScmObj* %lsts_4348062)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim57158, align 8
%stackaddr$prim57159 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48057, %struct.ScmObj* %anf_45bind48206)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim57159, align 8
%stackaddr$makeclosure57160 = alloca %struct.ScmObj*, align 8
%fptrToInt57161 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49341 to i64
%ae49341 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57161)
store volatile %struct.ScmObj* %ae49341, %struct.ScmObj** %stackaddr$makeclosure57160, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %k48448, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %_37foldr148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %anf_45bind48205, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %vs48060, i64 4)
%stackaddr$prim57162 = alloca %struct.ScmObj*, align 8
%cpsargs48459 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49341, %struct.ScmObj* %anf_45bind48207)
store volatile %struct.ScmObj* %cpsargs48459, %struct.ScmObj** %stackaddr$prim57162, align 8
%clofunc57163 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48053)
musttail call tailcc void %clofunc57163(%struct.ScmObj* %_37foldr48053, %struct.ScmObj* %cpsargs48459)
ret void
}

define tailcc void @proc_clo$ae49341(%struct.ScmObj* %env$ae49341,%struct.ScmObj* %current_45args55365) {
%stackaddr$env-ref57164 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref57164
%stackaddr$env-ref57165 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 1)
store %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$env-ref57165
%stackaddr$env-ref57166 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 2)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref57166
%stackaddr$env-ref57167 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 3)
store %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$env-ref57167
%stackaddr$env-ref57168 = alloca %struct.ScmObj*, align 8
%vs48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 4)
store %struct.ScmObj* %vs48060, %struct.ScmObj** %stackaddr$env-ref57168
%stackaddr$prim57169 = alloca %struct.ScmObj*, align 8
%_95k48456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55365)
store volatile %struct.ScmObj* %_95k48456, %struct.ScmObj** %stackaddr$prim57169, align 8
%stackaddr$prim57170 = alloca %struct.ScmObj*, align 8
%current_45args55366 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55365)
store volatile %struct.ScmObj* %current_45args55366, %struct.ScmObj** %stackaddr$prim57170, align 8
%stackaddr$prim57171 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55366)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim57171, align 8
%ae49346 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57172 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48208, %struct.ScmObj* %ae49346)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim57172, align 8
%stackaddr$makeclosure57173 = alloca %struct.ScmObj*, align 8
%fptrToInt57174 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49348 to i64
%ae49348 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57174)
store volatile %struct.ScmObj* %ae49348, %struct.ScmObj** %stackaddr$makeclosure57173, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49348, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49348, %struct.ScmObj* %k48448, i64 1)
%argslist55371$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57175 = alloca %struct.ScmObj*, align 8
%argslist55371$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48060, %struct.ScmObj* %argslist55371$_37foldr1480470)
store volatile %struct.ScmObj* %argslist55371$_37foldr1480471, %struct.ScmObj** %stackaddr$prim57175, align 8
%stackaddr$prim57176 = alloca %struct.ScmObj*, align 8
%argslist55371$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48209, %struct.ScmObj* %argslist55371$_37foldr1480471)
store volatile %struct.ScmObj* %argslist55371$_37foldr1480472, %struct.ScmObj** %stackaddr$prim57176, align 8
%stackaddr$prim57177 = alloca %struct.ScmObj*, align 8
%argslist55371$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48205, %struct.ScmObj* %argslist55371$_37foldr1480472)
store volatile %struct.ScmObj* %argslist55371$_37foldr1480473, %struct.ScmObj** %stackaddr$prim57177, align 8
%stackaddr$prim57178 = alloca %struct.ScmObj*, align 8
%argslist55371$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49348, %struct.ScmObj* %argslist55371$_37foldr1480473)
store volatile %struct.ScmObj* %argslist55371$_37foldr1480474, %struct.ScmObj** %stackaddr$prim57178, align 8
%clofunc57179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc57179(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist55371$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae49348(%struct.ScmObj* %env$ae49348,%struct.ScmObj* %current_45args55368) {
%stackaddr$env-ref57180 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49348, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref57180
%stackaddr$env-ref57181 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49348, i64 1)
store %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$env-ref57181
%stackaddr$prim57182 = alloca %struct.ScmObj*, align 8
%_95k48457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55368)
store volatile %struct.ScmObj* %_95k48457, %struct.ScmObj** %stackaddr$prim57182, align 8
%stackaddr$prim57183 = alloca %struct.ScmObj*, align 8
%current_45args55369 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55368)
store volatile %struct.ScmObj* %current_45args55369, %struct.ScmObj** %stackaddr$prim57183, align 8
%stackaddr$prim57184 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55369)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim57184, align 8
%stackaddr$prim57185 = alloca %struct.ScmObj*, align 8
%cpsargs48458 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48448, %struct.ScmObj* %anf_45bind48210)
store volatile %struct.ScmObj* %cpsargs48458, %struct.ScmObj** %stackaddr$prim57185, align 8
%clofunc57186 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48057)
musttail call tailcc void %clofunc57186(%struct.ScmObj* %f48057, %struct.ScmObj* %cpsargs48458)
ret void
}

define tailcc void @proc_clo$ae49317(%struct.ScmObj* %env$ae49317,%struct.ScmObj* %current_45args55372) {
%stackaddr$prim57187 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55372)
store volatile %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$prim57187, align 8
%stackaddr$prim57188 = alloca %struct.ScmObj*, align 8
%current_45args55373 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55372)
store volatile %struct.ScmObj* %current_45args55373, %struct.ScmObj** %stackaddr$prim57188, align 8
%stackaddr$prim57189 = alloca %struct.ScmObj*, align 8
%a48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55373)
store volatile %struct.ScmObj* %a48065, %struct.ScmObj** %stackaddr$prim57189, align 8
%stackaddr$prim57190 = alloca %struct.ScmObj*, align 8
%current_45args55374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55373)
store volatile %struct.ScmObj* %current_45args55374, %struct.ScmObj** %stackaddr$prim57190, align 8
%stackaddr$prim57191 = alloca %struct.ScmObj*, align 8
%b48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55374)
store volatile %struct.ScmObj* %b48064, %struct.ScmObj** %stackaddr$prim57191, align 8
%stackaddr$prim57192 = alloca %struct.ScmObj*, align 8
%cpsprim48461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48065, %struct.ScmObj* %b48064)
store volatile %struct.ScmObj* %cpsprim48461, %struct.ScmObj** %stackaddr$prim57192, align 8
%ae49321 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55376$k484600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57193 = alloca %struct.ScmObj*, align 8
%argslist55376$k484601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48461, %struct.ScmObj* %argslist55376$k484600)
store volatile %struct.ScmObj* %argslist55376$k484601, %struct.ScmObj** %stackaddr$prim57193, align 8
%stackaddr$prim57194 = alloca %struct.ScmObj*, align 8
%argslist55376$k484602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49321, %struct.ScmObj* %argslist55376$k484601)
store volatile %struct.ScmObj* %argslist55376$k484602, %struct.ScmObj** %stackaddr$prim57194, align 8
%clofunc57195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48460)
musttail call tailcc void %clofunc57195(%struct.ScmObj* %k48460, %struct.ScmObj* %argslist55376$k484602)
ret void
}

define tailcc void @proc_clo$ae49293(%struct.ScmObj* %env$ae49293,%struct.ScmObj* %current_45args55379) {
%stackaddr$prim57196 = alloca %struct.ScmObj*, align 8
%k48462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55379)
store volatile %struct.ScmObj* %k48462, %struct.ScmObj** %stackaddr$prim57196, align 8
%stackaddr$prim57197 = alloca %struct.ScmObj*, align 8
%current_45args55380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55379)
store volatile %struct.ScmObj* %current_45args55380, %struct.ScmObj** %stackaddr$prim57197, align 8
%stackaddr$prim57198 = alloca %struct.ScmObj*, align 8
%x48061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55380)
store volatile %struct.ScmObj* %x48061, %struct.ScmObj** %stackaddr$prim57198, align 8
%stackaddr$prim57199 = alloca %struct.ScmObj*, align 8
%cpsprim48463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48061)
store volatile %struct.ScmObj* %cpsprim48463, %struct.ScmObj** %stackaddr$prim57199, align 8
%ae49296 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55382$k484620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57200 = alloca %struct.ScmObj*, align 8
%argslist55382$k484621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48463, %struct.ScmObj* %argslist55382$k484620)
store volatile %struct.ScmObj* %argslist55382$k484621, %struct.ScmObj** %stackaddr$prim57200, align 8
%stackaddr$prim57201 = alloca %struct.ScmObj*, align 8
%argslist55382$k484622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49296, %struct.ScmObj* %argslist55382$k484621)
store volatile %struct.ScmObj* %argslist55382$k484622, %struct.ScmObj** %stackaddr$prim57201, align 8
%clofunc57202 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48462)
musttail call tailcc void %clofunc57202(%struct.ScmObj* %k48462, %struct.ScmObj* %argslist55382$k484622)
ret void
}

define tailcc void @proc_clo$ae49269(%struct.ScmObj* %env$ae49269,%struct.ScmObj* %current_45args55385) {
%stackaddr$prim57203 = alloca %struct.ScmObj*, align 8
%k48464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55385)
store volatile %struct.ScmObj* %k48464, %struct.ScmObj** %stackaddr$prim57203, align 8
%stackaddr$prim57204 = alloca %struct.ScmObj*, align 8
%current_45args55386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55385)
store volatile %struct.ScmObj* %current_45args55386, %struct.ScmObj** %stackaddr$prim57204, align 8
%stackaddr$prim57205 = alloca %struct.ScmObj*, align 8
%x48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55386)
store volatile %struct.ScmObj* %x48063, %struct.ScmObj** %stackaddr$prim57205, align 8
%stackaddr$prim57206 = alloca %struct.ScmObj*, align 8
%cpsprim48465 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48063)
store volatile %struct.ScmObj* %cpsprim48465, %struct.ScmObj** %stackaddr$prim57206, align 8
%ae49272 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55388$k484640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57207 = alloca %struct.ScmObj*, align 8
%argslist55388$k484641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48465, %struct.ScmObj* %argslist55388$k484640)
store volatile %struct.ScmObj* %argslist55388$k484641, %struct.ScmObj** %stackaddr$prim57207, align 8
%stackaddr$prim57208 = alloca %struct.ScmObj*, align 8
%argslist55388$k484642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49272, %struct.ScmObj* %argslist55388$k484641)
store volatile %struct.ScmObj* %argslist55388$k484642, %struct.ScmObj** %stackaddr$prim57208, align 8
%clofunc57209 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48464)
musttail call tailcc void %clofunc57209(%struct.ScmObj* %k48464, %struct.ScmObj* %argslist55388$k484642)
ret void
}

define tailcc void @proc_clo$ae49221(%struct.ScmObj* %env$ae49221,%struct.ScmObj* %current_45args55391) {
%stackaddr$prim57210 = alloca %struct.ScmObj*, align 8
%k48466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55391)
store volatile %struct.ScmObj* %k48466, %struct.ScmObj** %stackaddr$prim57210, align 8
%stackaddr$prim57211 = alloca %struct.ScmObj*, align 8
%current_45args55392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55391)
store volatile %struct.ScmObj* %current_45args55392, %struct.ScmObj** %stackaddr$prim57211, align 8
%stackaddr$prim57212 = alloca %struct.ScmObj*, align 8
%lst48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55392)
store volatile %struct.ScmObj* %lst48059, %struct.ScmObj** %stackaddr$prim57212, align 8
%stackaddr$prim57213 = alloca %struct.ScmObj*, align 8
%current_45args55393 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55392)
store volatile %struct.ScmObj* %current_45args55393, %struct.ScmObj** %stackaddr$prim57213, align 8
%stackaddr$prim57214 = alloca %struct.ScmObj*, align 8
%b48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55393)
store volatile %struct.ScmObj* %b48058, %struct.ScmObj** %stackaddr$prim57214, align 8
%truthy$cmp57215 = call i64 @is_truthy_value(%struct.ScmObj* %b48058)
%cmp$cmp57215 = icmp eq i64 %truthy$cmp57215, 1
br i1 %cmp$cmp57215, label %truebranch$cmp57215, label %falsebranch$cmp57215
truebranch$cmp57215:
%ae49224 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55395$k484660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57216 = alloca %struct.ScmObj*, align 8
%argslist55395$k484661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48058, %struct.ScmObj* %argslist55395$k484660)
store volatile %struct.ScmObj* %argslist55395$k484661, %struct.ScmObj** %stackaddr$prim57216, align 8
%stackaddr$prim57217 = alloca %struct.ScmObj*, align 8
%argslist55395$k484662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49224, %struct.ScmObj* %argslist55395$k484661)
store volatile %struct.ScmObj* %argslist55395$k484662, %struct.ScmObj** %stackaddr$prim57217, align 8
%clofunc57218 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48466)
musttail call tailcc void %clofunc57218(%struct.ScmObj* %k48466, %struct.ScmObj* %argslist55395$k484662)
ret void
falsebranch$cmp57215:
%stackaddr$prim57219 = alloca %struct.ScmObj*, align 8
%cpsprim48467 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48059)
store volatile %struct.ScmObj* %cpsprim48467, %struct.ScmObj** %stackaddr$prim57219, align 8
%ae49231 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55396$k484660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57220 = alloca %struct.ScmObj*, align 8
%argslist55396$k484661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48467, %struct.ScmObj* %argslist55396$k484660)
store volatile %struct.ScmObj* %argslist55396$k484661, %struct.ScmObj** %stackaddr$prim57220, align 8
%stackaddr$prim57221 = alloca %struct.ScmObj*, align 8
%argslist55396$k484662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49231, %struct.ScmObj* %argslist55396$k484661)
store volatile %struct.ScmObj* %argslist55396$k484662, %struct.ScmObj** %stackaddr$prim57221, align 8
%clofunc57222 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48466)
musttail call tailcc void %clofunc57222(%struct.ScmObj* %k48466, %struct.ScmObj* %argslist55396$k484662)
ret void
}

define tailcc void @proc_clo$ae49178(%struct.ScmObj* %env$ae49178,%struct.ScmObj* %current_45args55400) {
%stackaddr$env-ref57223 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49178, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref57223
%stackaddr$env-ref57224 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49178, i64 1)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref57224
%stackaddr$prim57225 = alloca %struct.ScmObj*, align 8
%k48468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55400)
store volatile %struct.ScmObj* %k48468, %struct.ScmObj** %stackaddr$prim57225, align 8
%stackaddr$prim57226 = alloca %struct.ScmObj*, align 8
%current_45args55401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55400)
store volatile %struct.ScmObj* %current_45args55401, %struct.ScmObj** %stackaddr$prim57226, align 8
%stackaddr$prim57227 = alloca %struct.ScmObj*, align 8
%lst48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55401)
store volatile %struct.ScmObj* %lst48068, %struct.ScmObj** %stackaddr$prim57227, align 8
%stackaddr$prim57228 = alloca %struct.ScmObj*, align 8
%current_45args55402 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55401)
store volatile %struct.ScmObj* %current_45args55402, %struct.ScmObj** %stackaddr$prim57228, align 8
%stackaddr$prim57229 = alloca %struct.ScmObj*, align 8
%n48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55402)
store volatile %struct.ScmObj* %n48067, %struct.ScmObj** %stackaddr$prim57229, align 8
%stackaddr$makeclosure57230 = alloca %struct.ScmObj*, align 8
%fptrToInt57231 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49180 to i64
%ae49180 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57231)
store volatile %struct.ScmObj* %ae49180, %struct.ScmObj** %stackaddr$makeclosure57230, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49180, %struct.ScmObj* %lst48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49180, %struct.ScmObj* %k48468, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49180, %struct.ScmObj* %_37take48039, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49180, %struct.ScmObj* %n48067, i64 3)
%argslist55408$_37length480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57232 = alloca %struct.ScmObj*, align 8
%argslist55408$_37length480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48068, %struct.ScmObj* %argslist55408$_37length480360)
store volatile %struct.ScmObj* %argslist55408$_37length480361, %struct.ScmObj** %stackaddr$prim57232, align 8
%stackaddr$prim57233 = alloca %struct.ScmObj*, align 8
%argslist55408$_37length480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49180, %struct.ScmObj* %argslist55408$_37length480361)
store volatile %struct.ScmObj* %argslist55408$_37length480362, %struct.ScmObj** %stackaddr$prim57233, align 8
%clofunc57234 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48036)
musttail call tailcc void %clofunc57234(%struct.ScmObj* %_37length48036, %struct.ScmObj* %argslist55408$_37length480362)
ret void
}

define tailcc void @proc_clo$ae49180(%struct.ScmObj* %env$ae49180,%struct.ScmObj* %current_45args55404) {
%stackaddr$env-ref57235 = alloca %struct.ScmObj*, align 8
%lst48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49180, i64 0)
store %struct.ScmObj* %lst48068, %struct.ScmObj** %stackaddr$env-ref57235
%stackaddr$env-ref57236 = alloca %struct.ScmObj*, align 8
%k48468 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49180, i64 1)
store %struct.ScmObj* %k48468, %struct.ScmObj** %stackaddr$env-ref57236
%stackaddr$env-ref57237 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49180, i64 2)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref57237
%stackaddr$env-ref57238 = alloca %struct.ScmObj*, align 8
%n48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49180, i64 3)
store %struct.ScmObj* %n48067, %struct.ScmObj** %stackaddr$env-ref57238
%stackaddr$prim57239 = alloca %struct.ScmObj*, align 8
%_95k48469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55404)
store volatile %struct.ScmObj* %_95k48469, %struct.ScmObj** %stackaddr$prim57239, align 8
%stackaddr$prim57240 = alloca %struct.ScmObj*, align 8
%current_45args55405 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55404)
store volatile %struct.ScmObj* %current_45args55405, %struct.ScmObj** %stackaddr$prim57240, align 8
%stackaddr$prim57241 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55405)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim57241, align 8
%stackaddr$prim57242 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48197, %struct.ScmObj* %n48067)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim57242, align 8
%argslist55407$_37take480390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57243 = alloca %struct.ScmObj*, align 8
%argslist55407$_37take480391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48198, %struct.ScmObj* %argslist55407$_37take480390)
store volatile %struct.ScmObj* %argslist55407$_37take480391, %struct.ScmObj** %stackaddr$prim57243, align 8
%stackaddr$prim57244 = alloca %struct.ScmObj*, align 8
%argslist55407$_37take480392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48068, %struct.ScmObj* %argslist55407$_37take480391)
store volatile %struct.ScmObj* %argslist55407$_37take480392, %struct.ScmObj** %stackaddr$prim57244, align 8
%stackaddr$prim57245 = alloca %struct.ScmObj*, align 8
%argslist55407$_37take480393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48468, %struct.ScmObj* %argslist55407$_37take480392)
store volatile %struct.ScmObj* %argslist55407$_37take480393, %struct.ScmObj** %stackaddr$prim57245, align 8
%clofunc57246 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48039)
musttail call tailcc void %clofunc57246(%struct.ScmObj* %_37take48039, %struct.ScmObj* %argslist55407$_37take480393)
ret void
}

define tailcc void @proc_clo$ae49124(%struct.ScmObj* %env$ae49124,%struct.ScmObj* %current_45args55410) {
%stackaddr$env-ref57247 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49124, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref57247
%stackaddr$prim57248 = alloca %struct.ScmObj*, align 8
%k48470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55410)
store volatile %struct.ScmObj* %k48470, %struct.ScmObj** %stackaddr$prim57248, align 8
%stackaddr$prim57249 = alloca %struct.ScmObj*, align 8
%current_45args55411 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55410)
store volatile %struct.ScmObj* %current_45args55411, %struct.ScmObj** %stackaddr$prim57249, align 8
%stackaddr$prim57250 = alloca %struct.ScmObj*, align 8
%lst48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55411)
store volatile %struct.ScmObj* %lst48070, %struct.ScmObj** %stackaddr$prim57250, align 8
%stackaddr$makeclosure57251 = alloca %struct.ScmObj*, align 8
%fptrToInt57252 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49125 to i64
%ae49125 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57252)
store volatile %struct.ScmObj* %ae49125, %struct.ScmObj** %stackaddr$makeclosure57251, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49125, %struct.ScmObj* %lst48070, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49125, %struct.ScmObj* %k48470, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49125, %struct.ScmObj* %_37foldl148031, i64 2)
%ae49126 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57253 = alloca %struct.ScmObj*, align 8
%fptrToInt57254 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49127 to i64
%ae49127 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57254)
store volatile %struct.ScmObj* %ae49127, %struct.ScmObj** %stackaddr$makeclosure57253, align 8
%argslist55422$ae491250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57255 = alloca %struct.ScmObj*, align 8
%argslist55422$ae491251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49127, %struct.ScmObj* %argslist55422$ae491250)
store volatile %struct.ScmObj* %argslist55422$ae491251, %struct.ScmObj** %stackaddr$prim57255, align 8
%stackaddr$prim57256 = alloca %struct.ScmObj*, align 8
%argslist55422$ae491252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49126, %struct.ScmObj* %argslist55422$ae491251)
store volatile %struct.ScmObj* %argslist55422$ae491252, %struct.ScmObj** %stackaddr$prim57256, align 8
%clofunc57257 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49125)
musttail call tailcc void %clofunc57257(%struct.ScmObj* %ae49125, %struct.ScmObj* %argslist55422$ae491252)
ret void
}

define tailcc void @proc_clo$ae49125(%struct.ScmObj* %env$ae49125,%struct.ScmObj* %current_45args55413) {
%stackaddr$env-ref57258 = alloca %struct.ScmObj*, align 8
%lst48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49125, i64 0)
store %struct.ScmObj* %lst48070, %struct.ScmObj** %stackaddr$env-ref57258
%stackaddr$env-ref57259 = alloca %struct.ScmObj*, align 8
%k48470 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49125, i64 1)
store %struct.ScmObj* %k48470, %struct.ScmObj** %stackaddr$env-ref57259
%stackaddr$env-ref57260 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49125, i64 2)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref57260
%stackaddr$prim57261 = alloca %struct.ScmObj*, align 8
%_95k48471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55413)
store volatile %struct.ScmObj* %_95k48471, %struct.ScmObj** %stackaddr$prim57261, align 8
%stackaddr$prim57262 = alloca %struct.ScmObj*, align 8
%current_45args55414 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55413)
store volatile %struct.ScmObj* %current_45args55414, %struct.ScmObj** %stackaddr$prim57262, align 8
%stackaddr$prim57263 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55414)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim57263, align 8
%ae49146 = call %struct.ScmObj* @const_init_null()
%argslist55416$_37foldl1480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57264 = alloca %struct.ScmObj*, align 8
%argslist55416$_37foldl1480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48070, %struct.ScmObj* %argslist55416$_37foldl1480310)
store volatile %struct.ScmObj* %argslist55416$_37foldl1480311, %struct.ScmObj** %stackaddr$prim57264, align 8
%stackaddr$prim57265 = alloca %struct.ScmObj*, align 8
%argslist55416$_37foldl1480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49146, %struct.ScmObj* %argslist55416$_37foldl1480311)
store volatile %struct.ScmObj* %argslist55416$_37foldl1480312, %struct.ScmObj** %stackaddr$prim57265, align 8
%stackaddr$prim57266 = alloca %struct.ScmObj*, align 8
%argslist55416$_37foldl1480313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48196, %struct.ScmObj* %argslist55416$_37foldl1480312)
store volatile %struct.ScmObj* %argslist55416$_37foldl1480313, %struct.ScmObj** %stackaddr$prim57266, align 8
%stackaddr$prim57267 = alloca %struct.ScmObj*, align 8
%argslist55416$_37foldl1480314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48470, %struct.ScmObj* %argslist55416$_37foldl1480313)
store volatile %struct.ScmObj* %argslist55416$_37foldl1480314, %struct.ScmObj** %stackaddr$prim57267, align 8
%clofunc57268 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148031)
musttail call tailcc void %clofunc57268(%struct.ScmObj* %_37foldl148031, %struct.ScmObj* %argslist55416$_37foldl1480314)
ret void
}

define tailcc void @proc_clo$ae49127(%struct.ScmObj* %env$ae49127,%struct.ScmObj* %current_45args55417) {
%stackaddr$prim57269 = alloca %struct.ScmObj*, align 8
%k48472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55417)
store volatile %struct.ScmObj* %k48472, %struct.ScmObj** %stackaddr$prim57269, align 8
%stackaddr$prim57270 = alloca %struct.ScmObj*, align 8
%current_45args55418 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55417)
store volatile %struct.ScmObj* %current_45args55418, %struct.ScmObj** %stackaddr$prim57270, align 8
%stackaddr$prim57271 = alloca %struct.ScmObj*, align 8
%x48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55418)
store volatile %struct.ScmObj* %x48072, %struct.ScmObj** %stackaddr$prim57271, align 8
%stackaddr$prim57272 = alloca %struct.ScmObj*, align 8
%current_45args55419 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55418)
store volatile %struct.ScmObj* %current_45args55419, %struct.ScmObj** %stackaddr$prim57272, align 8
%stackaddr$prim57273 = alloca %struct.ScmObj*, align 8
%y48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55419)
store volatile %struct.ScmObj* %y48071, %struct.ScmObj** %stackaddr$prim57273, align 8
%ae49129 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55421$k484720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57274 = alloca %struct.ScmObj*, align 8
%argslist55421$k484721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48072, %struct.ScmObj* %argslist55421$k484720)
store volatile %struct.ScmObj* %argslist55421$k484721, %struct.ScmObj** %stackaddr$prim57274, align 8
%stackaddr$prim57275 = alloca %struct.ScmObj*, align 8
%argslist55421$k484722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49129, %struct.ScmObj* %argslist55421$k484721)
store volatile %struct.ScmObj* %argslist55421$k484722, %struct.ScmObj** %stackaddr$prim57275, align 8
%clofunc57276 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48472)
musttail call tailcc void %clofunc57276(%struct.ScmObj* %k48472, %struct.ScmObj* %argslist55421$k484722)
ret void
}

define tailcc void @proc_clo$ae49045(%struct.ScmObj* %env$ae49045,%struct.ScmObj* %current_45args55425) {
%stackaddr$prim57277 = alloca %struct.ScmObj*, align 8
%k48473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55425)
store volatile %struct.ScmObj* %k48473, %struct.ScmObj** %stackaddr$prim57277, align 8
%stackaddr$prim57278 = alloca %struct.ScmObj*, align 8
%current_45args55426 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55425)
store volatile %struct.ScmObj* %current_45args55426, %struct.ScmObj** %stackaddr$prim57278, align 8
%stackaddr$prim57279 = alloca %struct.ScmObj*, align 8
%_37foldl148032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55426)
store volatile %struct.ScmObj* %_37foldl148032, %struct.ScmObj** %stackaddr$prim57279, align 8
%ae49047 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57280 = alloca %struct.ScmObj*, align 8
%fptrToInt57281 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49048 to i64
%ae49048 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57281)
store volatile %struct.ScmObj* %ae49048, %struct.ScmObj** %stackaddr$makeclosure57280, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49048, %struct.ScmObj* %_37foldl148032, i64 0)
%argslist55439$k484730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57282 = alloca %struct.ScmObj*, align 8
%argslist55439$k484731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49048, %struct.ScmObj* %argslist55439$k484730)
store volatile %struct.ScmObj* %argslist55439$k484731, %struct.ScmObj** %stackaddr$prim57282, align 8
%stackaddr$prim57283 = alloca %struct.ScmObj*, align 8
%argslist55439$k484732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49047, %struct.ScmObj* %argslist55439$k484731)
store volatile %struct.ScmObj* %argslist55439$k484732, %struct.ScmObj** %stackaddr$prim57283, align 8
%clofunc57284 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48473)
musttail call tailcc void %clofunc57284(%struct.ScmObj* %k48473, %struct.ScmObj* %argslist55439$k484732)
ret void
}

define tailcc void @proc_clo$ae49048(%struct.ScmObj* %env$ae49048,%struct.ScmObj* %current_45args55428) {
%stackaddr$env-ref57285 = alloca %struct.ScmObj*, align 8
%_37foldl148032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49048, i64 0)
store %struct.ScmObj* %_37foldl148032, %struct.ScmObj** %stackaddr$env-ref57285
%stackaddr$prim57286 = alloca %struct.ScmObj*, align 8
%k48474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55428)
store volatile %struct.ScmObj* %k48474, %struct.ScmObj** %stackaddr$prim57286, align 8
%stackaddr$prim57287 = alloca %struct.ScmObj*, align 8
%current_45args55429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55428)
store volatile %struct.ScmObj* %current_45args55429, %struct.ScmObj** %stackaddr$prim57287, align 8
%stackaddr$prim57288 = alloca %struct.ScmObj*, align 8
%f48035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55429)
store volatile %struct.ScmObj* %f48035, %struct.ScmObj** %stackaddr$prim57288, align 8
%stackaddr$prim57289 = alloca %struct.ScmObj*, align 8
%current_45args55430 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55429)
store volatile %struct.ScmObj* %current_45args55430, %struct.ScmObj** %stackaddr$prim57289, align 8
%stackaddr$prim57290 = alloca %struct.ScmObj*, align 8
%acc48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55430)
store volatile %struct.ScmObj* %acc48034, %struct.ScmObj** %stackaddr$prim57290, align 8
%stackaddr$prim57291 = alloca %struct.ScmObj*, align 8
%current_45args55431 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55430)
store volatile %struct.ScmObj* %current_45args55431, %struct.ScmObj** %stackaddr$prim57291, align 8
%stackaddr$prim57292 = alloca %struct.ScmObj*, align 8
%lst48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55431)
store volatile %struct.ScmObj* %lst48033, %struct.ScmObj** %stackaddr$prim57292, align 8
%stackaddr$prim57293 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48033)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim57293, align 8
%truthy$cmp57294 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48191)
%cmp$cmp57294 = icmp eq i64 %truthy$cmp57294, 1
br i1 %cmp$cmp57294, label %truebranch$cmp57294, label %falsebranch$cmp57294
truebranch$cmp57294:
%ae49052 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55433$k484740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57295 = alloca %struct.ScmObj*, align 8
%argslist55433$k484741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48034, %struct.ScmObj* %argslist55433$k484740)
store volatile %struct.ScmObj* %argslist55433$k484741, %struct.ScmObj** %stackaddr$prim57295, align 8
%stackaddr$prim57296 = alloca %struct.ScmObj*, align 8
%argslist55433$k484742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49052, %struct.ScmObj* %argslist55433$k484741)
store volatile %struct.ScmObj* %argslist55433$k484742, %struct.ScmObj** %stackaddr$prim57296, align 8
%clofunc57297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48474)
musttail call tailcc void %clofunc57297(%struct.ScmObj* %k48474, %struct.ScmObj* %argslist55433$k484742)
ret void
falsebranch$cmp57294:
%stackaddr$prim57298 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48033)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim57298, align 8
%stackaddr$makeclosure57299 = alloca %struct.ScmObj*, align 8
%fptrToInt57300 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49059 to i64
%ae49059 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57300)
store volatile %struct.ScmObj* %ae49059, %struct.ScmObj** %stackaddr$makeclosure57299, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49059, %struct.ScmObj* %f48035, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49059, %struct.ScmObj* %lst48033, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49059, %struct.ScmObj* %_37foldl148032, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49059, %struct.ScmObj* %k48474, i64 3)
%argslist55438$f480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57301 = alloca %struct.ScmObj*, align 8
%argslist55438$f480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48034, %struct.ScmObj* %argslist55438$f480350)
store volatile %struct.ScmObj* %argslist55438$f480351, %struct.ScmObj** %stackaddr$prim57301, align 8
%stackaddr$prim57302 = alloca %struct.ScmObj*, align 8
%argslist55438$f480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48192, %struct.ScmObj* %argslist55438$f480351)
store volatile %struct.ScmObj* %argslist55438$f480352, %struct.ScmObj** %stackaddr$prim57302, align 8
%stackaddr$prim57303 = alloca %struct.ScmObj*, align 8
%argslist55438$f480353 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49059, %struct.ScmObj* %argslist55438$f480352)
store volatile %struct.ScmObj* %argslist55438$f480353, %struct.ScmObj** %stackaddr$prim57303, align 8
%clofunc57304 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48035)
musttail call tailcc void %clofunc57304(%struct.ScmObj* %f48035, %struct.ScmObj* %argslist55438$f480353)
ret void
}

define tailcc void @proc_clo$ae49059(%struct.ScmObj* %env$ae49059,%struct.ScmObj* %current_45args55434) {
%stackaddr$env-ref57305 = alloca %struct.ScmObj*, align 8
%f48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49059, i64 0)
store %struct.ScmObj* %f48035, %struct.ScmObj** %stackaddr$env-ref57305
%stackaddr$env-ref57306 = alloca %struct.ScmObj*, align 8
%lst48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49059, i64 1)
store %struct.ScmObj* %lst48033, %struct.ScmObj** %stackaddr$env-ref57306
%stackaddr$env-ref57307 = alloca %struct.ScmObj*, align 8
%_37foldl148032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49059, i64 2)
store %struct.ScmObj* %_37foldl148032, %struct.ScmObj** %stackaddr$env-ref57307
%stackaddr$env-ref57308 = alloca %struct.ScmObj*, align 8
%k48474 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49059, i64 3)
store %struct.ScmObj* %k48474, %struct.ScmObj** %stackaddr$env-ref57308
%stackaddr$prim57309 = alloca %struct.ScmObj*, align 8
%_95k48475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55434)
store volatile %struct.ScmObj* %_95k48475, %struct.ScmObj** %stackaddr$prim57309, align 8
%stackaddr$prim57310 = alloca %struct.ScmObj*, align 8
%current_45args55435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55434)
store volatile %struct.ScmObj* %current_45args55435, %struct.ScmObj** %stackaddr$prim57310, align 8
%stackaddr$prim57311 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55435)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim57311, align 8
%stackaddr$prim57312 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48033)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim57312, align 8
%argslist55437$_37foldl1480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57313 = alloca %struct.ScmObj*, align 8
%argslist55437$_37foldl1480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48194, %struct.ScmObj* %argslist55437$_37foldl1480320)
store volatile %struct.ScmObj* %argslist55437$_37foldl1480321, %struct.ScmObj** %stackaddr$prim57313, align 8
%stackaddr$prim57314 = alloca %struct.ScmObj*, align 8
%argslist55437$_37foldl1480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48193, %struct.ScmObj* %argslist55437$_37foldl1480321)
store volatile %struct.ScmObj* %argslist55437$_37foldl1480322, %struct.ScmObj** %stackaddr$prim57314, align 8
%stackaddr$prim57315 = alloca %struct.ScmObj*, align 8
%argslist55437$_37foldl1480323 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48035, %struct.ScmObj* %argslist55437$_37foldl1480322)
store volatile %struct.ScmObj* %argslist55437$_37foldl1480323, %struct.ScmObj** %stackaddr$prim57315, align 8
%stackaddr$prim57316 = alloca %struct.ScmObj*, align 8
%argslist55437$_37foldl1480324 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48474, %struct.ScmObj* %argslist55437$_37foldl1480323)
store volatile %struct.ScmObj* %argslist55437$_37foldl1480324, %struct.ScmObj** %stackaddr$prim57316, align 8
%clofunc57317 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148032)
musttail call tailcc void %clofunc57317(%struct.ScmObj* %_37foldl148032, %struct.ScmObj* %argslist55437$_37foldl1480324)
ret void
}

define tailcc void @proc_clo$ae48962(%struct.ScmObj* %env$ae48962,%struct.ScmObj* %current_45args55442) {
%stackaddr$prim57318 = alloca %struct.ScmObj*, align 8
%k48476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55442)
store volatile %struct.ScmObj* %k48476, %struct.ScmObj** %stackaddr$prim57318, align 8
%stackaddr$prim57319 = alloca %struct.ScmObj*, align 8
%current_45args55443 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55442)
store volatile %struct.ScmObj* %current_45args55443, %struct.ScmObj** %stackaddr$prim57319, align 8
%stackaddr$prim57320 = alloca %struct.ScmObj*, align 8
%_37length48037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55443)
store volatile %struct.ScmObj* %_37length48037, %struct.ScmObj** %stackaddr$prim57320, align 8
%ae48964 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57321 = alloca %struct.ScmObj*, align 8
%fptrToInt57322 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48965 to i64
%ae48965 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57322)
store volatile %struct.ScmObj* %ae48965, %struct.ScmObj** %stackaddr$makeclosure57321, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48965, %struct.ScmObj* %_37length48037, i64 0)
%argslist55454$k484760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57323 = alloca %struct.ScmObj*, align 8
%argslist55454$k484761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48965, %struct.ScmObj* %argslist55454$k484760)
store volatile %struct.ScmObj* %argslist55454$k484761, %struct.ScmObj** %stackaddr$prim57323, align 8
%stackaddr$prim57324 = alloca %struct.ScmObj*, align 8
%argslist55454$k484762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48964, %struct.ScmObj* %argslist55454$k484761)
store volatile %struct.ScmObj* %argslist55454$k484762, %struct.ScmObj** %stackaddr$prim57324, align 8
%clofunc57325 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48476)
musttail call tailcc void %clofunc57325(%struct.ScmObj* %k48476, %struct.ScmObj* %argslist55454$k484762)
ret void
}

define tailcc void @proc_clo$ae48965(%struct.ScmObj* %env$ae48965,%struct.ScmObj* %current_45args55445) {
%stackaddr$env-ref57326 = alloca %struct.ScmObj*, align 8
%_37length48037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48965, i64 0)
store %struct.ScmObj* %_37length48037, %struct.ScmObj** %stackaddr$env-ref57326
%stackaddr$prim57327 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55445)
store volatile %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$prim57327, align 8
%stackaddr$prim57328 = alloca %struct.ScmObj*, align 8
%current_45args55446 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55445)
store volatile %struct.ScmObj* %current_45args55446, %struct.ScmObj** %stackaddr$prim57328, align 8
%stackaddr$prim57329 = alloca %struct.ScmObj*, align 8
%lst48038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55446)
store volatile %struct.ScmObj* %lst48038, %struct.ScmObj** %stackaddr$prim57329, align 8
%stackaddr$prim57330 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48038)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim57330, align 8
%truthy$cmp57331 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48187)
%cmp$cmp57331 = icmp eq i64 %truthy$cmp57331, 1
br i1 %cmp$cmp57331, label %truebranch$cmp57331, label %falsebranch$cmp57331
truebranch$cmp57331:
%ae48969 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48970 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55448$k484770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57332 = alloca %struct.ScmObj*, align 8
%argslist55448$k484771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48970, %struct.ScmObj* %argslist55448$k484770)
store volatile %struct.ScmObj* %argslist55448$k484771, %struct.ScmObj** %stackaddr$prim57332, align 8
%stackaddr$prim57333 = alloca %struct.ScmObj*, align 8
%argslist55448$k484772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48969, %struct.ScmObj* %argslist55448$k484771)
store volatile %struct.ScmObj* %argslist55448$k484772, %struct.ScmObj** %stackaddr$prim57333, align 8
%clofunc57334 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48477)
musttail call tailcc void %clofunc57334(%struct.ScmObj* %k48477, %struct.ScmObj* %argslist55448$k484772)
ret void
falsebranch$cmp57331:
%stackaddr$prim57335 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48038)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim57335, align 8
%stackaddr$makeclosure57336 = alloca %struct.ScmObj*, align 8
%fptrToInt57337 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48979 to i64
%ae48979 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57337)
store volatile %struct.ScmObj* %ae48979, %struct.ScmObj** %stackaddr$makeclosure57336, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48979, %struct.ScmObj* %k48477, i64 0)
%argslist55453$_37length480370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57338 = alloca %struct.ScmObj*, align 8
%argslist55453$_37length480371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48188, %struct.ScmObj* %argslist55453$_37length480370)
store volatile %struct.ScmObj* %argslist55453$_37length480371, %struct.ScmObj** %stackaddr$prim57338, align 8
%stackaddr$prim57339 = alloca %struct.ScmObj*, align 8
%argslist55453$_37length480372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48979, %struct.ScmObj* %argslist55453$_37length480371)
store volatile %struct.ScmObj* %argslist55453$_37length480372, %struct.ScmObj** %stackaddr$prim57339, align 8
%clofunc57340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48037)
musttail call tailcc void %clofunc57340(%struct.ScmObj* %_37length48037, %struct.ScmObj* %argslist55453$_37length480372)
ret void
}

define tailcc void @proc_clo$ae48979(%struct.ScmObj* %env$ae48979,%struct.ScmObj* %current_45args55449) {
%stackaddr$env-ref57341 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48979, i64 0)
store %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$env-ref57341
%stackaddr$prim57342 = alloca %struct.ScmObj*, align 8
%_95k48478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55449)
store volatile %struct.ScmObj* %_95k48478, %struct.ScmObj** %stackaddr$prim57342, align 8
%stackaddr$prim57343 = alloca %struct.ScmObj*, align 8
%current_45args55450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55449)
store volatile %struct.ScmObj* %current_45args55450, %struct.ScmObj** %stackaddr$prim57343, align 8
%stackaddr$prim57344 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55450)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim57344, align 8
%ae48981 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57345 = alloca %struct.ScmObj*, align 8
%cpsprim48479 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48981, %struct.ScmObj* %anf_45bind48189)
store volatile %struct.ScmObj* %cpsprim48479, %struct.ScmObj** %stackaddr$prim57345, align 8
%ae48984 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55452$k484770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57346 = alloca %struct.ScmObj*, align 8
%argslist55452$k484771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48479, %struct.ScmObj* %argslist55452$k484770)
store volatile %struct.ScmObj* %argslist55452$k484771, %struct.ScmObj** %stackaddr$prim57346, align 8
%stackaddr$prim57347 = alloca %struct.ScmObj*, align 8
%argslist55452$k484772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48984, %struct.ScmObj* %argslist55452$k484771)
store volatile %struct.ScmObj* %argslist55452$k484772, %struct.ScmObj** %stackaddr$prim57347, align 8
%clofunc57348 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48477)
musttail call tailcc void %clofunc57348(%struct.ScmObj* %k48477, %struct.ScmObj* %argslist55452$k484772)
ret void
}

define tailcc void @proc_clo$ae48812(%struct.ScmObj* %env$ae48812,%struct.ScmObj* %current_45args55457) {
%stackaddr$prim57349 = alloca %struct.ScmObj*, align 8
%k48480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55457)
store volatile %struct.ScmObj* %k48480, %struct.ScmObj** %stackaddr$prim57349, align 8
%stackaddr$prim57350 = alloca %struct.ScmObj*, align 8
%current_45args55458 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55457)
store volatile %struct.ScmObj* %current_45args55458, %struct.ScmObj** %stackaddr$prim57350, align 8
%stackaddr$prim57351 = alloca %struct.ScmObj*, align 8
%_37take48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55458)
store volatile %struct.ScmObj* %_37take48040, %struct.ScmObj** %stackaddr$prim57351, align 8
%ae48814 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57352 = alloca %struct.ScmObj*, align 8
%fptrToInt57353 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48815 to i64
%ae48815 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57353)
store volatile %struct.ScmObj* %ae48815, %struct.ScmObj** %stackaddr$makeclosure57352, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48815, %struct.ScmObj* %_37take48040, i64 0)
%argslist55471$k484800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57354 = alloca %struct.ScmObj*, align 8
%argslist55471$k484801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48815, %struct.ScmObj* %argslist55471$k484800)
store volatile %struct.ScmObj* %argslist55471$k484801, %struct.ScmObj** %stackaddr$prim57354, align 8
%stackaddr$prim57355 = alloca %struct.ScmObj*, align 8
%argslist55471$k484802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48814, %struct.ScmObj* %argslist55471$k484801)
store volatile %struct.ScmObj* %argslist55471$k484802, %struct.ScmObj** %stackaddr$prim57355, align 8
%clofunc57356 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48480)
musttail call tailcc void %clofunc57356(%struct.ScmObj* %k48480, %struct.ScmObj* %argslist55471$k484802)
ret void
}

define tailcc void @proc_clo$ae48815(%struct.ScmObj* %env$ae48815,%struct.ScmObj* %current_45args55460) {
%stackaddr$env-ref57357 = alloca %struct.ScmObj*, align 8
%_37take48040 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48815, i64 0)
store %struct.ScmObj* %_37take48040, %struct.ScmObj** %stackaddr$env-ref57357
%stackaddr$prim57358 = alloca %struct.ScmObj*, align 8
%k48481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55460)
store volatile %struct.ScmObj* %k48481, %struct.ScmObj** %stackaddr$prim57358, align 8
%stackaddr$prim57359 = alloca %struct.ScmObj*, align 8
%current_45args55461 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55460)
store volatile %struct.ScmObj* %current_45args55461, %struct.ScmObj** %stackaddr$prim57359, align 8
%stackaddr$prim57360 = alloca %struct.ScmObj*, align 8
%lst48042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55461)
store volatile %struct.ScmObj* %lst48042, %struct.ScmObj** %stackaddr$prim57360, align 8
%stackaddr$prim57361 = alloca %struct.ScmObj*, align 8
%current_45args55462 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55461)
store volatile %struct.ScmObj* %current_45args55462, %struct.ScmObj** %stackaddr$prim57361, align 8
%stackaddr$prim57362 = alloca %struct.ScmObj*, align 8
%n48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55462)
store volatile %struct.ScmObj* %n48041, %struct.ScmObj** %stackaddr$prim57362, align 8
%ae48817 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57363 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48041, %struct.ScmObj* %ae48817)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim57363, align 8
%truthy$cmp57364 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48180)
%cmp$cmp57364 = icmp eq i64 %truthy$cmp57364, 1
br i1 %cmp$cmp57364, label %truebranch$cmp57364, label %falsebranch$cmp57364
truebranch$cmp57364:
%ae48820 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48821 = call %struct.ScmObj* @const_init_null()
%argslist55464$k484810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57365 = alloca %struct.ScmObj*, align 8
%argslist55464$k484811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48821, %struct.ScmObj* %argslist55464$k484810)
store volatile %struct.ScmObj* %argslist55464$k484811, %struct.ScmObj** %stackaddr$prim57365, align 8
%stackaddr$prim57366 = alloca %struct.ScmObj*, align 8
%argslist55464$k484812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48820, %struct.ScmObj* %argslist55464$k484811)
store volatile %struct.ScmObj* %argslist55464$k484812, %struct.ScmObj** %stackaddr$prim57366, align 8
%clofunc57367 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48481)
musttail call tailcc void %clofunc57367(%struct.ScmObj* %k48481, %struct.ScmObj* %argslist55464$k484812)
ret void
falsebranch$cmp57364:
%stackaddr$prim57368 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48042)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim57368, align 8
%truthy$cmp57369 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48181)
%cmp$cmp57369 = icmp eq i64 %truthy$cmp57369, 1
br i1 %cmp$cmp57369, label %truebranch$cmp57369, label %falsebranch$cmp57369
truebranch$cmp57369:
%ae48831 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48832 = call %struct.ScmObj* @const_init_null()
%argslist55465$k484810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57370 = alloca %struct.ScmObj*, align 8
%argslist55465$k484811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48832, %struct.ScmObj* %argslist55465$k484810)
store volatile %struct.ScmObj* %argslist55465$k484811, %struct.ScmObj** %stackaddr$prim57370, align 8
%stackaddr$prim57371 = alloca %struct.ScmObj*, align 8
%argslist55465$k484812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48831, %struct.ScmObj* %argslist55465$k484811)
store volatile %struct.ScmObj* %argslist55465$k484812, %struct.ScmObj** %stackaddr$prim57371, align 8
%clofunc57372 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48481)
musttail call tailcc void %clofunc57372(%struct.ScmObj* %k48481, %struct.ScmObj* %argslist55465$k484812)
ret void
falsebranch$cmp57369:
%stackaddr$prim57373 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48042)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim57373, align 8
%stackaddr$prim57374 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48042)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim57374, align 8
%ae48842 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57375 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48041, %struct.ScmObj* %ae48842)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim57375, align 8
%stackaddr$makeclosure57376 = alloca %struct.ScmObj*, align 8
%fptrToInt57377 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48844 to i64
%ae48844 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57377)
store volatile %struct.ScmObj* %ae48844, %struct.ScmObj** %stackaddr$makeclosure57376, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48844, %struct.ScmObj* %anf_45bind48182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48844, %struct.ScmObj* %k48481, i64 1)
%argslist55470$_37take480400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57378 = alloca %struct.ScmObj*, align 8
%argslist55470$_37take480401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48184, %struct.ScmObj* %argslist55470$_37take480400)
store volatile %struct.ScmObj* %argslist55470$_37take480401, %struct.ScmObj** %stackaddr$prim57378, align 8
%stackaddr$prim57379 = alloca %struct.ScmObj*, align 8
%argslist55470$_37take480402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48183, %struct.ScmObj* %argslist55470$_37take480401)
store volatile %struct.ScmObj* %argslist55470$_37take480402, %struct.ScmObj** %stackaddr$prim57379, align 8
%stackaddr$prim57380 = alloca %struct.ScmObj*, align 8
%argslist55470$_37take480403 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48844, %struct.ScmObj* %argslist55470$_37take480402)
store volatile %struct.ScmObj* %argslist55470$_37take480403, %struct.ScmObj** %stackaddr$prim57380, align 8
%clofunc57381 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48040)
musttail call tailcc void %clofunc57381(%struct.ScmObj* %_37take48040, %struct.ScmObj* %argslist55470$_37take480403)
ret void
}

define tailcc void @proc_clo$ae48844(%struct.ScmObj* %env$ae48844,%struct.ScmObj* %current_45args55466) {
%stackaddr$env-ref57382 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48844, i64 0)
store %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$env-ref57382
%stackaddr$env-ref57383 = alloca %struct.ScmObj*, align 8
%k48481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48844, i64 1)
store %struct.ScmObj* %k48481, %struct.ScmObj** %stackaddr$env-ref57383
%stackaddr$prim57384 = alloca %struct.ScmObj*, align 8
%_95k48482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55466)
store volatile %struct.ScmObj* %_95k48482, %struct.ScmObj** %stackaddr$prim57384, align 8
%stackaddr$prim57385 = alloca %struct.ScmObj*, align 8
%current_45args55467 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55466)
store volatile %struct.ScmObj* %current_45args55467, %struct.ScmObj** %stackaddr$prim57385, align 8
%stackaddr$prim57386 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55467)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim57386, align 8
%stackaddr$prim57387 = alloca %struct.ScmObj*, align 8
%cpsprim48483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48182, %struct.ScmObj* %anf_45bind48185)
store volatile %struct.ScmObj* %cpsprim48483, %struct.ScmObj** %stackaddr$prim57387, align 8
%ae48850 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55469$k484810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57388 = alloca %struct.ScmObj*, align 8
%argslist55469$k484811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48483, %struct.ScmObj* %argslist55469$k484810)
store volatile %struct.ScmObj* %argslist55469$k484811, %struct.ScmObj** %stackaddr$prim57388, align 8
%stackaddr$prim57389 = alloca %struct.ScmObj*, align 8
%argslist55469$k484812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48850, %struct.ScmObj* %argslist55469$k484811)
store volatile %struct.ScmObj* %argslist55469$k484812, %struct.ScmObj** %stackaddr$prim57389, align 8
%clofunc57390 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48481)
musttail call tailcc void %clofunc57390(%struct.ScmObj* %k48481, %struct.ScmObj* %argslist55469$k484812)
ret void
}

define tailcc void @proc_clo$ae48715(%struct.ScmObj* %env$ae48715,%struct.ScmObj* %current_45args55474) {
%stackaddr$prim57391 = alloca %struct.ScmObj*, align 8
%k48484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55474)
store volatile %struct.ScmObj* %k48484, %struct.ScmObj** %stackaddr$prim57391, align 8
%stackaddr$prim57392 = alloca %struct.ScmObj*, align 8
%current_45args55475 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55474)
store volatile %struct.ScmObj* %current_45args55475, %struct.ScmObj** %stackaddr$prim57392, align 8
%stackaddr$prim57393 = alloca %struct.ScmObj*, align 8
%_37map48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55475)
store volatile %struct.ScmObj* %_37map48044, %struct.ScmObj** %stackaddr$prim57393, align 8
%ae48717 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57394 = alloca %struct.ScmObj*, align 8
%fptrToInt57395 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48718 to i64
%ae48718 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57395)
store volatile %struct.ScmObj* %ae48718, %struct.ScmObj** %stackaddr$makeclosure57394, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48718, %struct.ScmObj* %_37map48044, i64 0)
%argslist55491$k484840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57396 = alloca %struct.ScmObj*, align 8
%argslist55491$k484841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48718, %struct.ScmObj* %argslist55491$k484840)
store volatile %struct.ScmObj* %argslist55491$k484841, %struct.ScmObj** %stackaddr$prim57396, align 8
%stackaddr$prim57397 = alloca %struct.ScmObj*, align 8
%argslist55491$k484842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48717, %struct.ScmObj* %argslist55491$k484841)
store volatile %struct.ScmObj* %argslist55491$k484842, %struct.ScmObj** %stackaddr$prim57397, align 8
%clofunc57398 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48484)
musttail call tailcc void %clofunc57398(%struct.ScmObj* %k48484, %struct.ScmObj* %argslist55491$k484842)
ret void
}

define tailcc void @proc_clo$ae48718(%struct.ScmObj* %env$ae48718,%struct.ScmObj* %current_45args55477) {
%stackaddr$env-ref57399 = alloca %struct.ScmObj*, align 8
%_37map48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48718, i64 0)
store %struct.ScmObj* %_37map48044, %struct.ScmObj** %stackaddr$env-ref57399
%stackaddr$prim57400 = alloca %struct.ScmObj*, align 8
%k48485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55477)
store volatile %struct.ScmObj* %k48485, %struct.ScmObj** %stackaddr$prim57400, align 8
%stackaddr$prim57401 = alloca %struct.ScmObj*, align 8
%current_45args55478 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55477)
store volatile %struct.ScmObj* %current_45args55478, %struct.ScmObj** %stackaddr$prim57401, align 8
%stackaddr$prim57402 = alloca %struct.ScmObj*, align 8
%f48046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55478)
store volatile %struct.ScmObj* %f48046, %struct.ScmObj** %stackaddr$prim57402, align 8
%stackaddr$prim57403 = alloca %struct.ScmObj*, align 8
%current_45args55479 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55478)
store volatile %struct.ScmObj* %current_45args55479, %struct.ScmObj** %stackaddr$prim57403, align 8
%stackaddr$prim57404 = alloca %struct.ScmObj*, align 8
%lst48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55479)
store volatile %struct.ScmObj* %lst48045, %struct.ScmObj** %stackaddr$prim57404, align 8
%stackaddr$prim57405 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim57405, align 8
%truthy$cmp57406 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48174)
%cmp$cmp57406 = icmp eq i64 %truthy$cmp57406, 1
br i1 %cmp$cmp57406, label %truebranch$cmp57406, label %falsebranch$cmp57406
truebranch$cmp57406:
%ae48722 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48723 = call %struct.ScmObj* @const_init_null()
%argslist55481$k484850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57407 = alloca %struct.ScmObj*, align 8
%argslist55481$k484851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48723, %struct.ScmObj* %argslist55481$k484850)
store volatile %struct.ScmObj* %argslist55481$k484851, %struct.ScmObj** %stackaddr$prim57407, align 8
%stackaddr$prim57408 = alloca %struct.ScmObj*, align 8
%argslist55481$k484852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48722, %struct.ScmObj* %argslist55481$k484851)
store volatile %struct.ScmObj* %argslist55481$k484852, %struct.ScmObj** %stackaddr$prim57408, align 8
%clofunc57409 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48485)
musttail call tailcc void %clofunc57409(%struct.ScmObj* %k48485, %struct.ScmObj* %argslist55481$k484852)
ret void
falsebranch$cmp57406:
%stackaddr$prim57410 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim57410, align 8
%stackaddr$makeclosure57411 = alloca %struct.ScmObj*, align 8
%fptrToInt57412 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48732 to i64
%ae48732 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57412)
store volatile %struct.ScmObj* %ae48732, %struct.ScmObj** %stackaddr$makeclosure57411, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48732, %struct.ScmObj* %k48485, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48732, %struct.ScmObj* %f48046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48732, %struct.ScmObj* %lst48045, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48732, %struct.ScmObj* %_37map48044, i64 3)
%argslist55490$f480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57413 = alloca %struct.ScmObj*, align 8
%argslist55490$f480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48175, %struct.ScmObj* %argslist55490$f480460)
store volatile %struct.ScmObj* %argslist55490$f480461, %struct.ScmObj** %stackaddr$prim57413, align 8
%stackaddr$prim57414 = alloca %struct.ScmObj*, align 8
%argslist55490$f480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48732, %struct.ScmObj* %argslist55490$f480461)
store volatile %struct.ScmObj* %argslist55490$f480462, %struct.ScmObj** %stackaddr$prim57414, align 8
%clofunc57415 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48046)
musttail call tailcc void %clofunc57415(%struct.ScmObj* %f48046, %struct.ScmObj* %argslist55490$f480462)
ret void
}

define tailcc void @proc_clo$ae48732(%struct.ScmObj* %env$ae48732,%struct.ScmObj* %current_45args55482) {
%stackaddr$env-ref57416 = alloca %struct.ScmObj*, align 8
%k48485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48732, i64 0)
store %struct.ScmObj* %k48485, %struct.ScmObj** %stackaddr$env-ref57416
%stackaddr$env-ref57417 = alloca %struct.ScmObj*, align 8
%f48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48732, i64 1)
store %struct.ScmObj* %f48046, %struct.ScmObj** %stackaddr$env-ref57417
%stackaddr$env-ref57418 = alloca %struct.ScmObj*, align 8
%lst48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48732, i64 2)
store %struct.ScmObj* %lst48045, %struct.ScmObj** %stackaddr$env-ref57418
%stackaddr$env-ref57419 = alloca %struct.ScmObj*, align 8
%_37map48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48732, i64 3)
store %struct.ScmObj* %_37map48044, %struct.ScmObj** %stackaddr$env-ref57419
%stackaddr$prim57420 = alloca %struct.ScmObj*, align 8
%_95k48486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55482)
store volatile %struct.ScmObj* %_95k48486, %struct.ScmObj** %stackaddr$prim57420, align 8
%stackaddr$prim57421 = alloca %struct.ScmObj*, align 8
%current_45args55483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55482)
store volatile %struct.ScmObj* %current_45args55483, %struct.ScmObj** %stackaddr$prim57421, align 8
%stackaddr$prim57422 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55483)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim57422, align 8
%stackaddr$prim57423 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim57423, align 8
%stackaddr$makeclosure57424 = alloca %struct.ScmObj*, align 8
%fptrToInt57425 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48736 to i64
%ae48736 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57425)
store volatile %struct.ScmObj* %ae48736, %struct.ScmObj** %stackaddr$makeclosure57424, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48736, %struct.ScmObj* %k48485, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48736, %struct.ScmObj* %anf_45bind48176, i64 1)
%argslist55489$_37map480440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57426 = alloca %struct.ScmObj*, align 8
%argslist55489$_37map480441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48177, %struct.ScmObj* %argslist55489$_37map480440)
store volatile %struct.ScmObj* %argslist55489$_37map480441, %struct.ScmObj** %stackaddr$prim57426, align 8
%stackaddr$prim57427 = alloca %struct.ScmObj*, align 8
%argslist55489$_37map480442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48046, %struct.ScmObj* %argslist55489$_37map480441)
store volatile %struct.ScmObj* %argslist55489$_37map480442, %struct.ScmObj** %stackaddr$prim57427, align 8
%stackaddr$prim57428 = alloca %struct.ScmObj*, align 8
%argslist55489$_37map480443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48736, %struct.ScmObj* %argslist55489$_37map480442)
store volatile %struct.ScmObj* %argslist55489$_37map480443, %struct.ScmObj** %stackaddr$prim57428, align 8
%clofunc57429 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48044)
musttail call tailcc void %clofunc57429(%struct.ScmObj* %_37map48044, %struct.ScmObj* %argslist55489$_37map480443)
ret void
}

define tailcc void @proc_clo$ae48736(%struct.ScmObj* %env$ae48736,%struct.ScmObj* %current_45args55485) {
%stackaddr$env-ref57430 = alloca %struct.ScmObj*, align 8
%k48485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48736, i64 0)
store %struct.ScmObj* %k48485, %struct.ScmObj** %stackaddr$env-ref57430
%stackaddr$env-ref57431 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48736, i64 1)
store %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$env-ref57431
%stackaddr$prim57432 = alloca %struct.ScmObj*, align 8
%_95k48487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55485)
store volatile %struct.ScmObj* %_95k48487, %struct.ScmObj** %stackaddr$prim57432, align 8
%stackaddr$prim57433 = alloca %struct.ScmObj*, align 8
%current_45args55486 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55485)
store volatile %struct.ScmObj* %current_45args55486, %struct.ScmObj** %stackaddr$prim57433, align 8
%stackaddr$prim57434 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55486)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim57434, align 8
%stackaddr$prim57435 = alloca %struct.ScmObj*, align 8
%cpsprim48488 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48176, %struct.ScmObj* %anf_45bind48178)
store volatile %struct.ScmObj* %cpsprim48488, %struct.ScmObj** %stackaddr$prim57435, align 8
%ae48742 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55488$k484850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57436 = alloca %struct.ScmObj*, align 8
%argslist55488$k484851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48488, %struct.ScmObj* %argslist55488$k484850)
store volatile %struct.ScmObj* %argslist55488$k484851, %struct.ScmObj** %stackaddr$prim57436, align 8
%stackaddr$prim57437 = alloca %struct.ScmObj*, align 8
%argslist55488$k484852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48742, %struct.ScmObj* %argslist55488$k484851)
store volatile %struct.ScmObj* %argslist55488$k484852, %struct.ScmObj** %stackaddr$prim57437, align 8
%clofunc57438 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48485)
musttail call tailcc void %clofunc57438(%struct.ScmObj* %k48485, %struct.ScmObj* %argslist55488$k484852)
ret void
}

define tailcc void @proc_clo$ae48635(%struct.ScmObj* %env$ae48635,%struct.ScmObj* %current_45args55494) {
%stackaddr$prim57439 = alloca %struct.ScmObj*, align 8
%k48489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55494)
store volatile %struct.ScmObj* %k48489, %struct.ScmObj** %stackaddr$prim57439, align 8
%stackaddr$prim57440 = alloca %struct.ScmObj*, align 8
%current_45args55495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55494)
store volatile %struct.ScmObj* %current_45args55495, %struct.ScmObj** %stackaddr$prim57440, align 8
%stackaddr$prim57441 = alloca %struct.ScmObj*, align 8
%_37foldr148048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55495)
store volatile %struct.ScmObj* %_37foldr148048, %struct.ScmObj** %stackaddr$prim57441, align 8
%ae48637 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57442 = alloca %struct.ScmObj*, align 8
%fptrToInt57443 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48638 to i64
%ae48638 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57443)
store volatile %struct.ScmObj* %ae48638, %struct.ScmObj** %stackaddr$makeclosure57442, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48638, %struct.ScmObj* %_37foldr148048, i64 0)
%argslist55508$k484890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57444 = alloca %struct.ScmObj*, align 8
%argslist55508$k484891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48638, %struct.ScmObj* %argslist55508$k484890)
store volatile %struct.ScmObj* %argslist55508$k484891, %struct.ScmObj** %stackaddr$prim57444, align 8
%stackaddr$prim57445 = alloca %struct.ScmObj*, align 8
%argslist55508$k484892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48637, %struct.ScmObj* %argslist55508$k484891)
store volatile %struct.ScmObj* %argslist55508$k484892, %struct.ScmObj** %stackaddr$prim57445, align 8
%clofunc57446 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48489)
musttail call tailcc void %clofunc57446(%struct.ScmObj* %k48489, %struct.ScmObj* %argslist55508$k484892)
ret void
}

define tailcc void @proc_clo$ae48638(%struct.ScmObj* %env$ae48638,%struct.ScmObj* %current_45args55497) {
%stackaddr$env-ref57447 = alloca %struct.ScmObj*, align 8
%_37foldr148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48638, i64 0)
store %struct.ScmObj* %_37foldr148048, %struct.ScmObj** %stackaddr$env-ref57447
%stackaddr$prim57448 = alloca %struct.ScmObj*, align 8
%k48490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55497)
store volatile %struct.ScmObj* %k48490, %struct.ScmObj** %stackaddr$prim57448, align 8
%stackaddr$prim57449 = alloca %struct.ScmObj*, align 8
%current_45args55498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55497)
store volatile %struct.ScmObj* %current_45args55498, %struct.ScmObj** %stackaddr$prim57449, align 8
%stackaddr$prim57450 = alloca %struct.ScmObj*, align 8
%f48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55498)
store volatile %struct.ScmObj* %f48051, %struct.ScmObj** %stackaddr$prim57450, align 8
%stackaddr$prim57451 = alloca %struct.ScmObj*, align 8
%current_45args55499 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55498)
store volatile %struct.ScmObj* %current_45args55499, %struct.ScmObj** %stackaddr$prim57451, align 8
%stackaddr$prim57452 = alloca %struct.ScmObj*, align 8
%acc48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55499)
store volatile %struct.ScmObj* %acc48050, %struct.ScmObj** %stackaddr$prim57452, align 8
%stackaddr$prim57453 = alloca %struct.ScmObj*, align 8
%current_45args55500 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55499)
store volatile %struct.ScmObj* %current_45args55500, %struct.ScmObj** %stackaddr$prim57453, align 8
%stackaddr$prim57454 = alloca %struct.ScmObj*, align 8
%lst48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55500)
store volatile %struct.ScmObj* %lst48049, %struct.ScmObj** %stackaddr$prim57454, align 8
%stackaddr$prim57455 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$prim57455, align 8
%truthy$cmp57456 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48169)
%cmp$cmp57456 = icmp eq i64 %truthy$cmp57456, 1
br i1 %cmp$cmp57456, label %truebranch$cmp57456, label %falsebranch$cmp57456
truebranch$cmp57456:
%ae48642 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55502$k484900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57457 = alloca %struct.ScmObj*, align 8
%argslist55502$k484901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48050, %struct.ScmObj* %argslist55502$k484900)
store volatile %struct.ScmObj* %argslist55502$k484901, %struct.ScmObj** %stackaddr$prim57457, align 8
%stackaddr$prim57458 = alloca %struct.ScmObj*, align 8
%argslist55502$k484902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48642, %struct.ScmObj* %argslist55502$k484901)
store volatile %struct.ScmObj* %argslist55502$k484902, %struct.ScmObj** %stackaddr$prim57458, align 8
%clofunc57459 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48490)
musttail call tailcc void %clofunc57459(%struct.ScmObj* %k48490, %struct.ScmObj* %argslist55502$k484902)
ret void
falsebranch$cmp57456:
%stackaddr$prim57460 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$prim57460, align 8
%stackaddr$prim57461 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$prim57461, align 8
%stackaddr$makeclosure57462 = alloca %struct.ScmObj*, align 8
%fptrToInt57463 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48650 to i64
%ae48650 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57463)
store volatile %struct.ScmObj* %ae48650, %struct.ScmObj** %stackaddr$makeclosure57462, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48650, %struct.ScmObj* %k48490, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48650, %struct.ScmObj* %anf_45bind48170, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48650, %struct.ScmObj* %f48051, i64 2)
%argslist55507$_37foldr1480480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57464 = alloca %struct.ScmObj*, align 8
%argslist55507$_37foldr1480481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48171, %struct.ScmObj* %argslist55507$_37foldr1480480)
store volatile %struct.ScmObj* %argslist55507$_37foldr1480481, %struct.ScmObj** %stackaddr$prim57464, align 8
%stackaddr$prim57465 = alloca %struct.ScmObj*, align 8
%argslist55507$_37foldr1480482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48050, %struct.ScmObj* %argslist55507$_37foldr1480481)
store volatile %struct.ScmObj* %argslist55507$_37foldr1480482, %struct.ScmObj** %stackaddr$prim57465, align 8
%stackaddr$prim57466 = alloca %struct.ScmObj*, align 8
%argslist55507$_37foldr1480483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48051, %struct.ScmObj* %argslist55507$_37foldr1480482)
store volatile %struct.ScmObj* %argslist55507$_37foldr1480483, %struct.ScmObj** %stackaddr$prim57466, align 8
%stackaddr$prim57467 = alloca %struct.ScmObj*, align 8
%argslist55507$_37foldr1480484 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48650, %struct.ScmObj* %argslist55507$_37foldr1480483)
store volatile %struct.ScmObj* %argslist55507$_37foldr1480484, %struct.ScmObj** %stackaddr$prim57467, align 8
%clofunc57468 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148048)
musttail call tailcc void %clofunc57468(%struct.ScmObj* %_37foldr148048, %struct.ScmObj* %argslist55507$_37foldr1480484)
ret void
}

define tailcc void @proc_clo$ae48650(%struct.ScmObj* %env$ae48650,%struct.ScmObj* %current_45args55503) {
%stackaddr$env-ref57469 = alloca %struct.ScmObj*, align 8
%k48490 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48650, i64 0)
store %struct.ScmObj* %k48490, %struct.ScmObj** %stackaddr$env-ref57469
%stackaddr$env-ref57470 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48650, i64 1)
store %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$env-ref57470
%stackaddr$env-ref57471 = alloca %struct.ScmObj*, align 8
%f48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48650, i64 2)
store %struct.ScmObj* %f48051, %struct.ScmObj** %stackaddr$env-ref57471
%stackaddr$prim57472 = alloca %struct.ScmObj*, align 8
%_95k48491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55503)
store volatile %struct.ScmObj* %_95k48491, %struct.ScmObj** %stackaddr$prim57472, align 8
%stackaddr$prim57473 = alloca %struct.ScmObj*, align 8
%current_45args55504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55503)
store volatile %struct.ScmObj* %current_45args55504, %struct.ScmObj** %stackaddr$prim57473, align 8
%stackaddr$prim57474 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55504)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim57474, align 8
%argslist55506$f480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57475 = alloca %struct.ScmObj*, align 8
%argslist55506$f480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48172, %struct.ScmObj* %argslist55506$f480510)
store volatile %struct.ScmObj* %argslist55506$f480511, %struct.ScmObj** %stackaddr$prim57475, align 8
%stackaddr$prim57476 = alloca %struct.ScmObj*, align 8
%argslist55506$f480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48170, %struct.ScmObj* %argslist55506$f480511)
store volatile %struct.ScmObj* %argslist55506$f480512, %struct.ScmObj** %stackaddr$prim57476, align 8
%stackaddr$prim57477 = alloca %struct.ScmObj*, align 8
%argslist55506$f480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48490, %struct.ScmObj* %argslist55506$f480512)
store volatile %struct.ScmObj* %argslist55506$f480513, %struct.ScmObj** %stackaddr$prim57477, align 8
%clofunc57478 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48051)
musttail call tailcc void %clofunc57478(%struct.ScmObj* %f48051, %struct.ScmObj* %argslist55506$f480513)
ret void
}

define tailcc void @proc_clo$ae48518(%struct.ScmObj* %env$ae48518,%struct.ScmObj* %current_45args55511) {
%stackaddr$prim57479 = alloca %struct.ScmObj*, align 8
%k48492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55511)
store volatile %struct.ScmObj* %k48492, %struct.ScmObj** %stackaddr$prim57479, align 8
%stackaddr$prim57480 = alloca %struct.ScmObj*, align 8
%current_45args55512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55511)
store volatile %struct.ScmObj* %current_45args55512, %struct.ScmObj** %stackaddr$prim57480, align 8
%stackaddr$prim57481 = alloca %struct.ScmObj*, align 8
%y48028 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55512)
store volatile %struct.ScmObj* %y48028, %struct.ScmObj** %stackaddr$prim57481, align 8
%ae48520 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57482 = alloca %struct.ScmObj*, align 8
%fptrToInt57483 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48521 to i64
%ae48521 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57483)
store volatile %struct.ScmObj* %ae48521, %struct.ScmObj** %stackaddr$makeclosure57482, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48521, %struct.ScmObj* %y48028, i64 0)
%argslist55530$k484920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57484 = alloca %struct.ScmObj*, align 8
%argslist55530$k484921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48521, %struct.ScmObj* %argslist55530$k484920)
store volatile %struct.ScmObj* %argslist55530$k484921, %struct.ScmObj** %stackaddr$prim57484, align 8
%stackaddr$prim57485 = alloca %struct.ScmObj*, align 8
%argslist55530$k484922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48520, %struct.ScmObj* %argslist55530$k484921)
store volatile %struct.ScmObj* %argslist55530$k484922, %struct.ScmObj** %stackaddr$prim57485, align 8
%clofunc57486 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48492)
musttail call tailcc void %clofunc57486(%struct.ScmObj* %k48492, %struct.ScmObj* %argslist55530$k484922)
ret void
}

define tailcc void @proc_clo$ae48521(%struct.ScmObj* %env$ae48521,%struct.ScmObj* %current_45args55514) {
%stackaddr$env-ref57487 = alloca %struct.ScmObj*, align 8
%y48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48521, i64 0)
store %struct.ScmObj* %y48028, %struct.ScmObj** %stackaddr$env-ref57487
%stackaddr$prim57488 = alloca %struct.ScmObj*, align 8
%k48493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55514)
store volatile %struct.ScmObj* %k48493, %struct.ScmObj** %stackaddr$prim57488, align 8
%stackaddr$prim57489 = alloca %struct.ScmObj*, align 8
%current_45args55515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55514)
store volatile %struct.ScmObj* %current_45args55515, %struct.ScmObj** %stackaddr$prim57489, align 8
%stackaddr$prim57490 = alloca %struct.ScmObj*, align 8
%f48029 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55515)
store volatile %struct.ScmObj* %f48029, %struct.ScmObj** %stackaddr$prim57490, align 8
%stackaddr$makeclosure57491 = alloca %struct.ScmObj*, align 8
%fptrToInt57492 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48522 to i64
%ae48522 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57492)
store volatile %struct.ScmObj* %ae48522, %struct.ScmObj** %stackaddr$makeclosure57491, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48522, %struct.ScmObj* %f48029, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48522, %struct.ScmObj* %k48493, i64 1)
%ae48523 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57493 = alloca %struct.ScmObj*, align 8
%fptrToInt57494 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48524 to i64
%ae48524 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57494)
store volatile %struct.ScmObj* %ae48524, %struct.ScmObj** %stackaddr$makeclosure57493, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48524, %struct.ScmObj* %f48029, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48524, %struct.ScmObj* %y48028, i64 1)
%argslist55529$ae485220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57495 = alloca %struct.ScmObj*, align 8
%argslist55529$ae485221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48524, %struct.ScmObj* %argslist55529$ae485220)
store volatile %struct.ScmObj* %argslist55529$ae485221, %struct.ScmObj** %stackaddr$prim57495, align 8
%stackaddr$prim57496 = alloca %struct.ScmObj*, align 8
%argslist55529$ae485222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48523, %struct.ScmObj* %argslist55529$ae485221)
store volatile %struct.ScmObj* %argslist55529$ae485222, %struct.ScmObj** %stackaddr$prim57496, align 8
%clofunc57497 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48522)
musttail call tailcc void %clofunc57497(%struct.ScmObj* %ae48522, %struct.ScmObj* %argslist55529$ae485222)
ret void
}

define tailcc void @proc_clo$ae48522(%struct.ScmObj* %env$ae48522,%struct.ScmObj* %current_45args55517) {
%stackaddr$env-ref57498 = alloca %struct.ScmObj*, align 8
%f48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48522, i64 0)
store %struct.ScmObj* %f48029, %struct.ScmObj** %stackaddr$env-ref57498
%stackaddr$env-ref57499 = alloca %struct.ScmObj*, align 8
%k48493 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48522, i64 1)
store %struct.ScmObj* %k48493, %struct.ScmObj** %stackaddr$env-ref57499
%stackaddr$prim57500 = alloca %struct.ScmObj*, align 8
%_95k48494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55517)
store volatile %struct.ScmObj* %_95k48494, %struct.ScmObj** %stackaddr$prim57500, align 8
%stackaddr$prim57501 = alloca %struct.ScmObj*, align 8
%current_45args55518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55517)
store volatile %struct.ScmObj* %current_45args55518, %struct.ScmObj** %stackaddr$prim57501, align 8
%stackaddr$prim57502 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55518)
store volatile %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$prim57502, align 8
%argslist55520$f480290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57503 = alloca %struct.ScmObj*, align 8
%argslist55520$f480291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48167, %struct.ScmObj* %argslist55520$f480290)
store volatile %struct.ScmObj* %argslist55520$f480291, %struct.ScmObj** %stackaddr$prim57503, align 8
%stackaddr$prim57504 = alloca %struct.ScmObj*, align 8
%argslist55520$f480292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48493, %struct.ScmObj* %argslist55520$f480291)
store volatile %struct.ScmObj* %argslist55520$f480292, %struct.ScmObj** %stackaddr$prim57504, align 8
%clofunc57505 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48029)
musttail call tailcc void %clofunc57505(%struct.ScmObj* %f48029, %struct.ScmObj* %argslist55520$f480292)
ret void
}

define tailcc void @proc_clo$ae48524(%struct.ScmObj* %env$ae48524,%struct.ScmObj* %args4803048495) {
%stackaddr$env-ref57506 = alloca %struct.ScmObj*, align 8
%f48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48524, i64 0)
store %struct.ScmObj* %f48029, %struct.ScmObj** %stackaddr$env-ref57506
%stackaddr$env-ref57507 = alloca %struct.ScmObj*, align 8
%y48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48524, i64 1)
store %struct.ScmObj* %y48028, %struct.ScmObj** %stackaddr$env-ref57507
%stackaddr$prim57508 = alloca %struct.ScmObj*, align 8
%k48496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4803048495)
store volatile %struct.ScmObj* %k48496, %struct.ScmObj** %stackaddr$prim57508, align 8
%stackaddr$prim57509 = alloca %struct.ScmObj*, align 8
%args48030 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4803048495)
store volatile %struct.ScmObj* %args48030, %struct.ScmObj** %stackaddr$prim57509, align 8
%stackaddr$makeclosure57510 = alloca %struct.ScmObj*, align 8
%fptrToInt57511 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48528 to i64
%ae48528 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57511)
store volatile %struct.ScmObj* %ae48528, %struct.ScmObj** %stackaddr$makeclosure57510, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48528, %struct.ScmObj* %k48496, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48528, %struct.ScmObj* %args48030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48528, %struct.ScmObj* %f48029, i64 2)
%argslist55528$y480280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57512 = alloca %struct.ScmObj*, align 8
%argslist55528$y480281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48028, %struct.ScmObj* %argslist55528$y480280)
store volatile %struct.ScmObj* %argslist55528$y480281, %struct.ScmObj** %stackaddr$prim57512, align 8
%stackaddr$prim57513 = alloca %struct.ScmObj*, align 8
%argslist55528$y480282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48528, %struct.ScmObj* %argslist55528$y480281)
store volatile %struct.ScmObj* %argslist55528$y480282, %struct.ScmObj** %stackaddr$prim57513, align 8
%clofunc57514 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48028)
musttail call tailcc void %clofunc57514(%struct.ScmObj* %y48028, %struct.ScmObj* %argslist55528$y480282)
ret void
}

define tailcc void @proc_clo$ae48528(%struct.ScmObj* %env$ae48528,%struct.ScmObj* %current_45args55521) {
%stackaddr$env-ref57515 = alloca %struct.ScmObj*, align 8
%k48496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48528, i64 0)
store %struct.ScmObj* %k48496, %struct.ScmObj** %stackaddr$env-ref57515
%stackaddr$env-ref57516 = alloca %struct.ScmObj*, align 8
%args48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48528, i64 1)
store %struct.ScmObj* %args48030, %struct.ScmObj** %stackaddr$env-ref57516
%stackaddr$env-ref57517 = alloca %struct.ScmObj*, align 8
%f48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48528, i64 2)
store %struct.ScmObj* %f48029, %struct.ScmObj** %stackaddr$env-ref57517
%stackaddr$prim57518 = alloca %struct.ScmObj*, align 8
%_95k48497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55521)
store volatile %struct.ScmObj* %_95k48497, %struct.ScmObj** %stackaddr$prim57518, align 8
%stackaddr$prim57519 = alloca %struct.ScmObj*, align 8
%current_45args55522 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55521)
store volatile %struct.ScmObj* %current_45args55522, %struct.ScmObj** %stackaddr$prim57519, align 8
%stackaddr$prim57520 = alloca %struct.ScmObj*, align 8
%anf_45bind48165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55522)
store volatile %struct.ScmObj* %anf_45bind48165, %struct.ScmObj** %stackaddr$prim57520, align 8
%stackaddr$makeclosure57521 = alloca %struct.ScmObj*, align 8
%fptrToInt57522 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48531 to i64
%ae48531 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57522)
store volatile %struct.ScmObj* %ae48531, %struct.ScmObj** %stackaddr$makeclosure57521, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48531, %struct.ScmObj* %k48496, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48531, %struct.ScmObj* %args48030, i64 1)
%argslist55527$anf_45bind481650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57523 = alloca %struct.ScmObj*, align 8
%argslist55527$anf_45bind481651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48029, %struct.ScmObj* %argslist55527$anf_45bind481650)
store volatile %struct.ScmObj* %argslist55527$anf_45bind481651, %struct.ScmObj** %stackaddr$prim57523, align 8
%stackaddr$prim57524 = alloca %struct.ScmObj*, align 8
%argslist55527$anf_45bind481652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48531, %struct.ScmObj* %argslist55527$anf_45bind481651)
store volatile %struct.ScmObj* %argslist55527$anf_45bind481652, %struct.ScmObj** %stackaddr$prim57524, align 8
%clofunc57525 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48165)
musttail call tailcc void %clofunc57525(%struct.ScmObj* %anf_45bind48165, %struct.ScmObj* %argslist55527$anf_45bind481652)
ret void
}

define tailcc void @proc_clo$ae48531(%struct.ScmObj* %env$ae48531,%struct.ScmObj* %current_45args55524) {
%stackaddr$env-ref57526 = alloca %struct.ScmObj*, align 8
%k48496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48531, i64 0)
store %struct.ScmObj* %k48496, %struct.ScmObj** %stackaddr$env-ref57526
%stackaddr$env-ref57527 = alloca %struct.ScmObj*, align 8
%args48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48531, i64 1)
store %struct.ScmObj* %args48030, %struct.ScmObj** %stackaddr$env-ref57527
%stackaddr$prim57528 = alloca %struct.ScmObj*, align 8
%_95k48498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55524)
store volatile %struct.ScmObj* %_95k48498, %struct.ScmObj** %stackaddr$prim57528, align 8
%stackaddr$prim57529 = alloca %struct.ScmObj*, align 8
%current_45args55525 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55524)
store volatile %struct.ScmObj* %current_45args55525, %struct.ScmObj** %stackaddr$prim57529, align 8
%stackaddr$prim57530 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55525)
store volatile %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$prim57530, align 8
%stackaddr$prim57531 = alloca %struct.ScmObj*, align 8
%cpsargs48499 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48496, %struct.ScmObj* %args48030)
store volatile %struct.ScmObj* %cpsargs48499, %struct.ScmObj** %stackaddr$prim57531, align 8
%clofunc57532 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48166)
musttail call tailcc void %clofunc57532(%struct.ScmObj* %anf_45bind48166, %struct.ScmObj* %cpsargs48499)
ret void
}

define tailcc void @proc_clo$ae48503(%struct.ScmObj* %env$ae48503,%struct.ScmObj* %current_45args55532) {
%stackaddr$prim57533 = alloca %struct.ScmObj*, align 8
%k48500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55532)
store volatile %struct.ScmObj* %k48500, %struct.ScmObj** %stackaddr$prim57533, align 8
%stackaddr$prim57534 = alloca %struct.ScmObj*, align 8
%current_45args55533 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55532)
store volatile %struct.ScmObj* %current_45args55533, %struct.ScmObj** %stackaddr$prim57534, align 8
%stackaddr$prim57535 = alloca %struct.ScmObj*, align 8
%yu48027 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55533)
store volatile %struct.ScmObj* %yu48027, %struct.ScmObj** %stackaddr$prim57535, align 8
%argslist55535$yu480270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57536 = alloca %struct.ScmObj*, align 8
%argslist55535$yu480271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48027, %struct.ScmObj* %argslist55535$yu480270)
store volatile %struct.ScmObj* %argslist55535$yu480271, %struct.ScmObj** %stackaddr$prim57536, align 8
%stackaddr$prim57537 = alloca %struct.ScmObj*, align 8
%argslist55535$yu480272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48500, %struct.ScmObj* %argslist55535$yu480271)
store volatile %struct.ScmObj* %argslist55535$yu480272, %struct.ScmObj** %stackaddr$prim57537, align 8
%clofunc57538 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48027)
musttail call tailcc void %clofunc57538(%struct.ScmObj* %yu48027, %struct.ScmObj* %argslist55535$yu480272)
ret void
}