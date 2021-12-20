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
%mainenv54582 = call %struct.ScmObj* @const_init_null()
%mainargs54583 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv54582, %struct.ScmObj* %mainargs54583)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv54580,%struct.ScmObj* %mainargs54581) {
%stackaddr$makeclosure54584 = alloca %struct.ScmObj*, align 8
%fptrToInt54585 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47544 to i64
%ae47544 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54585)
store volatile %struct.ScmObj* %ae47544, %struct.ScmObj** %stackaddr$makeclosure54584, align 8
%ae47545 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54586 = alloca %struct.ScmObj*, align 8
%fptrToInt54587 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47546 to i64
%ae47546 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54587)
store volatile %struct.ScmObj* %ae47546, %struct.ScmObj** %stackaddr$makeclosure54586, align 8
%argslist54579$ae475440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54588 = alloca %struct.ScmObj*, align 8
%argslist54579$ae475441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47546, %struct.ScmObj* %argslist54579$ae475440)
store volatile %struct.ScmObj* %argslist54579$ae475441, %struct.ScmObj** %stackaddr$prim54588, align 8
%stackaddr$prim54589 = alloca %struct.ScmObj*, align 8
%argslist54579$ae475442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47545, %struct.ScmObj* %argslist54579$ae475441)
store volatile %struct.ScmObj* %argslist54579$ae475442, %struct.ScmObj** %stackaddr$prim54589, align 8
%clofunc54590 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47544)
musttail call tailcc void %clofunc54590(%struct.ScmObj* %ae47544, %struct.ScmObj* %argslist54579$ae475442)
ret void
}

define tailcc void @proc_clo$ae47544(%struct.ScmObj* %env$ae47544,%struct.ScmObj* %current_45args53957) {
%stackaddr$prim54591 = alloca %struct.ScmObj*, align 8
%_95k47342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53957)
store volatile %struct.ScmObj* %_95k47342, %struct.ScmObj** %stackaddr$prim54591, align 8
%stackaddr$prim54592 = alloca %struct.ScmObj*, align 8
%current_45args53958 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53957)
store volatile %struct.ScmObj* %current_45args53958, %struct.ScmObj** %stackaddr$prim54592, align 8
%stackaddr$prim54593 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53958)
store volatile %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$prim54593, align 8
%stackaddr$makeclosure54594 = alloca %struct.ScmObj*, align 8
%fptrToInt54595 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47559 to i64
%ae47559 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54595)
store volatile %struct.ScmObj* %ae47559, %struct.ScmObj** %stackaddr$makeclosure54594, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47559, %struct.ScmObj* %anf_45bind47207, i64 0)
%ae47560 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54596 = alloca %struct.ScmObj*, align 8
%fptrToInt54597 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47561 to i64
%ae47561 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54597)
store volatile %struct.ScmObj* %ae47561, %struct.ScmObj** %stackaddr$makeclosure54596, align 8
%argslist54574$ae475590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54598 = alloca %struct.ScmObj*, align 8
%argslist54574$ae475591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47561, %struct.ScmObj* %argslist54574$ae475590)
store volatile %struct.ScmObj* %argslist54574$ae475591, %struct.ScmObj** %stackaddr$prim54598, align 8
%stackaddr$prim54599 = alloca %struct.ScmObj*, align 8
%argslist54574$ae475592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47560, %struct.ScmObj* %argslist54574$ae475591)
store volatile %struct.ScmObj* %argslist54574$ae475592, %struct.ScmObj** %stackaddr$prim54599, align 8
%clofunc54600 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47559)
musttail call tailcc void %clofunc54600(%struct.ScmObj* %ae47559, %struct.ScmObj* %argslist54574$ae475592)
ret void
}

define tailcc void @proc_clo$ae47559(%struct.ScmObj* %env$ae47559,%struct.ScmObj* %current_45args53960) {
%stackaddr$env-ref54601 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47559, i64 0)
store %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$env-ref54601
%stackaddr$prim54602 = alloca %struct.ScmObj*, align 8
%_95k47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53960)
store volatile %struct.ScmObj* %_95k47343, %struct.ScmObj** %stackaddr$prim54602, align 8
%stackaddr$prim54603 = alloca %struct.ScmObj*, align 8
%current_45args53961 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53960)
store volatile %struct.ScmObj* %current_45args53961, %struct.ScmObj** %stackaddr$prim54603, align 8
%stackaddr$prim54604 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53961)
store volatile %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$prim54604, align 8
%stackaddr$makeclosure54605 = alloca %struct.ScmObj*, align 8
%fptrToInt54606 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47674 to i64
%ae47674 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54606)
store volatile %struct.ScmObj* %ae47674, %struct.ScmObj** %stackaddr$makeclosure54605, align 8
%argslist54553$anf_45bind472070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54607 = alloca %struct.ScmObj*, align 8
%argslist54553$anf_45bind472071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47211, %struct.ScmObj* %argslist54553$anf_45bind472070)
store volatile %struct.ScmObj* %argslist54553$anf_45bind472071, %struct.ScmObj** %stackaddr$prim54607, align 8
%stackaddr$prim54608 = alloca %struct.ScmObj*, align 8
%argslist54553$anf_45bind472072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47674, %struct.ScmObj* %argslist54553$anf_45bind472071)
store volatile %struct.ScmObj* %argslist54553$anf_45bind472072, %struct.ScmObj** %stackaddr$prim54608, align 8
%clofunc54609 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47207)
musttail call tailcc void %clofunc54609(%struct.ScmObj* %anf_45bind47207, %struct.ScmObj* %argslist54553$anf_45bind472072)
ret void
}

define tailcc void @proc_clo$ae47674(%struct.ScmObj* %env$ae47674,%struct.ScmObj* %current_45args53963) {
%stackaddr$prim54610 = alloca %struct.ScmObj*, align 8
%_95k47344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53963)
store volatile %struct.ScmObj* %_95k47344, %struct.ScmObj** %stackaddr$prim54610, align 8
%stackaddr$prim54611 = alloca %struct.ScmObj*, align 8
%current_45args53964 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53963)
store volatile %struct.ScmObj* %current_45args53964, %struct.ScmObj** %stackaddr$prim54611, align 8
%stackaddr$prim54612 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53964)
store volatile %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$prim54612, align 8
%stackaddr$makeclosure54613 = alloca %struct.ScmObj*, align 8
%fptrToInt54614 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47676 to i64
%ae47676 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54614)
store volatile %struct.ScmObj* %ae47676, %struct.ScmObj** %stackaddr$makeclosure54613, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47676, %struct.ScmObj* %Ycmb47069, i64 0)
%ae47677 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54615 = alloca %struct.ScmObj*, align 8
%fptrToInt54616 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47678 to i64
%ae47678 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54616)
store volatile %struct.ScmObj* %ae47678, %struct.ScmObj** %stackaddr$makeclosure54615, align 8
%argslist54552$ae476760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54617 = alloca %struct.ScmObj*, align 8
%argslist54552$ae476761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47678, %struct.ScmObj* %argslist54552$ae476760)
store volatile %struct.ScmObj* %argslist54552$ae476761, %struct.ScmObj** %stackaddr$prim54617, align 8
%stackaddr$prim54618 = alloca %struct.ScmObj*, align 8
%argslist54552$ae476762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47677, %struct.ScmObj* %argslist54552$ae476761)
store volatile %struct.ScmObj* %argslist54552$ae476762, %struct.ScmObj** %stackaddr$prim54618, align 8
%clofunc54619 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47676)
musttail call tailcc void %clofunc54619(%struct.ScmObj* %ae47676, %struct.ScmObj* %argslist54552$ae476762)
ret void
}

define tailcc void @proc_clo$ae47676(%struct.ScmObj* %env$ae47676,%struct.ScmObj* %current_45args53966) {
%stackaddr$env-ref54620 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47676, i64 0)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54620
%stackaddr$prim54621 = alloca %struct.ScmObj*, align 8
%_95k47345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53966)
store volatile %struct.ScmObj* %_95k47345, %struct.ScmObj** %stackaddr$prim54621, align 8
%stackaddr$prim54622 = alloca %struct.ScmObj*, align 8
%current_45args53967 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53966)
store volatile %struct.ScmObj* %current_45args53967, %struct.ScmObj** %stackaddr$prim54622, align 8
%stackaddr$prim54623 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53967)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim54623, align 8
%stackaddr$makeclosure54624 = alloca %struct.ScmObj*, align 8
%fptrToInt54625 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47754 to i64
%ae47754 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54625)
store volatile %struct.ScmObj* %ae47754, %struct.ScmObj** %stackaddr$makeclosure54624, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47754, %struct.ScmObj* %Ycmb47069, i64 0)
%argslist54536$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54626 = alloca %struct.ScmObj*, align 8
%argslist54536$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47216, %struct.ScmObj* %argslist54536$Ycmb470690)
store volatile %struct.ScmObj* %argslist54536$Ycmb470691, %struct.ScmObj** %stackaddr$prim54626, align 8
%stackaddr$prim54627 = alloca %struct.ScmObj*, align 8
%argslist54536$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47754, %struct.ScmObj* %argslist54536$Ycmb470691)
store volatile %struct.ScmObj* %argslist54536$Ycmb470692, %struct.ScmObj** %stackaddr$prim54627, align 8
%clofunc54628 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc54628(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist54536$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae47754(%struct.ScmObj* %env$ae47754,%struct.ScmObj* %current_45args53969) {
%stackaddr$env-ref54629 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47754, i64 0)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54629
%stackaddr$prim54630 = alloca %struct.ScmObj*, align 8
%_95k47346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53969)
store volatile %struct.ScmObj* %_95k47346, %struct.ScmObj** %stackaddr$prim54630, align 8
%stackaddr$prim54631 = alloca %struct.ScmObj*, align 8
%current_45args53970 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53969)
store volatile %struct.ScmObj* %current_45args53970, %struct.ScmObj** %stackaddr$prim54631, align 8
%stackaddr$prim54632 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53970)
store volatile %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$prim54632, align 8
%stackaddr$makeclosure54633 = alloca %struct.ScmObj*, align 8
%fptrToInt54634 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47756 to i64
%ae47756 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54634)
store volatile %struct.ScmObj* %ae47756, %struct.ScmObj** %stackaddr$makeclosure54633, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47756, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47756, %struct.ScmObj* %Ycmb47069, i64 1)
%ae47757 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54635 = alloca %struct.ScmObj*, align 8
%fptrToInt54636 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47758 to i64
%ae47758 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54636)
store volatile %struct.ScmObj* %ae47758, %struct.ScmObj** %stackaddr$makeclosure54635, align 8
%argslist54535$ae477560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54637 = alloca %struct.ScmObj*, align 8
%argslist54535$ae477561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47758, %struct.ScmObj* %argslist54535$ae477560)
store volatile %struct.ScmObj* %argslist54535$ae477561, %struct.ScmObj** %stackaddr$prim54637, align 8
%stackaddr$prim54638 = alloca %struct.ScmObj*, align 8
%argslist54535$ae477562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47757, %struct.ScmObj* %argslist54535$ae477561)
store volatile %struct.ScmObj* %argslist54535$ae477562, %struct.ScmObj** %stackaddr$prim54638, align 8
%clofunc54639 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47756)
musttail call tailcc void %clofunc54639(%struct.ScmObj* %ae47756, %struct.ScmObj* %argslist54535$ae477562)
ret void
}

define tailcc void @proc_clo$ae47756(%struct.ScmObj* %env$ae47756,%struct.ScmObj* %current_45args53972) {
%stackaddr$env-ref54640 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47756, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54640
%stackaddr$env-ref54641 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47756, i64 1)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54641
%stackaddr$prim54642 = alloca %struct.ScmObj*, align 8
%_95k47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53972)
store volatile %struct.ScmObj* %_95k47347, %struct.ScmObj** %stackaddr$prim54642, align 8
%stackaddr$prim54643 = alloca %struct.ScmObj*, align 8
%current_45args53973 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53972)
store volatile %struct.ScmObj* %current_45args53973, %struct.ScmObj** %stackaddr$prim54643, align 8
%stackaddr$prim54644 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53973)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim54644, align 8
%stackaddr$makeclosure54645 = alloca %struct.ScmObj*, align 8
%fptrToInt54646 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47851 to i64
%ae47851 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54646)
store volatile %struct.ScmObj* %ae47851, %struct.ScmObj** %stackaddr$makeclosure54645, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47851, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47851, %struct.ScmObj* %Ycmb47069, i64 1)
%argslist54516$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54647 = alloca %struct.ScmObj*, align 8
%argslist54516$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47222, %struct.ScmObj* %argslist54516$Ycmb470690)
store volatile %struct.ScmObj* %argslist54516$Ycmb470691, %struct.ScmObj** %stackaddr$prim54647, align 8
%stackaddr$prim54648 = alloca %struct.ScmObj*, align 8
%argslist54516$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47851, %struct.ScmObj* %argslist54516$Ycmb470691)
store volatile %struct.ScmObj* %argslist54516$Ycmb470692, %struct.ScmObj** %stackaddr$prim54648, align 8
%clofunc54649 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc54649(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist54516$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae47851(%struct.ScmObj* %env$ae47851,%struct.ScmObj* %current_45args53975) {
%stackaddr$env-ref54650 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47851, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54650
%stackaddr$env-ref54651 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47851, i64 1)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54651
%stackaddr$prim54652 = alloca %struct.ScmObj*, align 8
%_95k47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53975)
store volatile %struct.ScmObj* %_95k47348, %struct.ScmObj** %stackaddr$prim54652, align 8
%stackaddr$prim54653 = alloca %struct.ScmObj*, align 8
%current_45args53976 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53975)
store volatile %struct.ScmObj* %current_45args53976, %struct.ScmObj** %stackaddr$prim54653, align 8
%stackaddr$prim54654 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53976)
store volatile %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$prim54654, align 8
%stackaddr$makeclosure54655 = alloca %struct.ScmObj*, align 8
%fptrToInt54656 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47853 to i64
%ae47853 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54656)
store volatile %struct.ScmObj* %ae47853, %struct.ScmObj** %stackaddr$makeclosure54655, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47853, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47853, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47853, %struct.ScmObj* %Ycmb47069, i64 2)
%ae47854 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54657 = alloca %struct.ScmObj*, align 8
%fptrToInt54658 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47855 to i64
%ae47855 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54658)
store volatile %struct.ScmObj* %ae47855, %struct.ScmObj** %stackaddr$makeclosure54657, align 8
%argslist54515$ae478530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54659 = alloca %struct.ScmObj*, align 8
%argslist54515$ae478531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47855, %struct.ScmObj* %argslist54515$ae478530)
store volatile %struct.ScmObj* %argslist54515$ae478531, %struct.ScmObj** %stackaddr$prim54659, align 8
%stackaddr$prim54660 = alloca %struct.ScmObj*, align 8
%argslist54515$ae478532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47854, %struct.ScmObj* %argslist54515$ae478531)
store volatile %struct.ScmObj* %argslist54515$ae478532, %struct.ScmObj** %stackaddr$prim54660, align 8
%clofunc54661 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47853)
musttail call tailcc void %clofunc54661(%struct.ScmObj* %ae47853, %struct.ScmObj* %argslist54515$ae478532)
ret void
}

define tailcc void @proc_clo$ae47853(%struct.ScmObj* %env$ae47853,%struct.ScmObj* %current_45args53978) {
%stackaddr$env-ref54662 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47853, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54662
%stackaddr$env-ref54663 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47853, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54663
%stackaddr$env-ref54664 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47853, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54664
%stackaddr$prim54665 = alloca %struct.ScmObj*, align 8
%_95k47349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53978)
store volatile %struct.ScmObj* %_95k47349, %struct.ScmObj** %stackaddr$prim54665, align 8
%stackaddr$prim54666 = alloca %struct.ScmObj*, align 8
%current_45args53979 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53978)
store volatile %struct.ScmObj* %current_45args53979, %struct.ScmObj** %stackaddr$prim54666, align 8
%stackaddr$prim54667 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53979)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim54667, align 8
%stackaddr$makeclosure54668 = alloca %struct.ScmObj*, align 8
%fptrToInt54669 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48001 to i64
%ae48001 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54669)
store volatile %struct.ScmObj* %ae48001, %struct.ScmObj** %stackaddr$makeclosure54668, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48001, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48001, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48001, %struct.ScmObj* %Ycmb47069, i64 2)
%argslist54499$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54670 = alloca %struct.ScmObj*, align 8
%argslist54499$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47229, %struct.ScmObj* %argslist54499$Ycmb470690)
store volatile %struct.ScmObj* %argslist54499$Ycmb470691, %struct.ScmObj** %stackaddr$prim54670, align 8
%stackaddr$prim54671 = alloca %struct.ScmObj*, align 8
%argslist54499$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48001, %struct.ScmObj* %argslist54499$Ycmb470691)
store volatile %struct.ScmObj* %argslist54499$Ycmb470692, %struct.ScmObj** %stackaddr$prim54671, align 8
%clofunc54672 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc54672(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist54499$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae48001(%struct.ScmObj* %env$ae48001,%struct.ScmObj* %current_45args53981) {
%stackaddr$env-ref54673 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48001, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54673
%stackaddr$env-ref54674 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48001, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54674
%stackaddr$env-ref54675 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48001, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54675
%stackaddr$prim54676 = alloca %struct.ScmObj*, align 8
%_95k47350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53981)
store volatile %struct.ScmObj* %_95k47350, %struct.ScmObj** %stackaddr$prim54676, align 8
%stackaddr$prim54677 = alloca %struct.ScmObj*, align 8
%current_45args53982 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53981)
store volatile %struct.ScmObj* %current_45args53982, %struct.ScmObj** %stackaddr$prim54677, align 8
%stackaddr$prim54678 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53982)
store volatile %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$prim54678, align 8
%stackaddr$makeclosure54679 = alloca %struct.ScmObj*, align 8
%fptrToInt54680 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48003 to i64
%ae48003 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54680)
store volatile %struct.ScmObj* %ae48003, %struct.ScmObj** %stackaddr$makeclosure54679, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48003, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48003, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48003, %struct.ScmObj* %Ycmb47069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48003, %struct.ScmObj* %_37take47082, i64 3)
%ae48004 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54681 = alloca %struct.ScmObj*, align 8
%fptrToInt54682 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48005 to i64
%ae48005 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54682)
store volatile %struct.ScmObj* %ae48005, %struct.ScmObj** %stackaddr$makeclosure54681, align 8
%argslist54498$ae480030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54683 = alloca %struct.ScmObj*, align 8
%argslist54498$ae480031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48005, %struct.ScmObj* %argslist54498$ae480030)
store volatile %struct.ScmObj* %argslist54498$ae480031, %struct.ScmObj** %stackaddr$prim54683, align 8
%stackaddr$prim54684 = alloca %struct.ScmObj*, align 8
%argslist54498$ae480032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48004, %struct.ScmObj* %argslist54498$ae480031)
store volatile %struct.ScmObj* %argslist54498$ae480032, %struct.ScmObj** %stackaddr$prim54684, align 8
%clofunc54685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48003)
musttail call tailcc void %clofunc54685(%struct.ScmObj* %ae48003, %struct.ScmObj* %argslist54498$ae480032)
ret void
}

define tailcc void @proc_clo$ae48003(%struct.ScmObj* %env$ae48003,%struct.ScmObj* %current_45args53984) {
%stackaddr$env-ref54686 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48003, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54686
%stackaddr$env-ref54687 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48003, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54687
%stackaddr$env-ref54688 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48003, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54688
%stackaddr$env-ref54689 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48003, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref54689
%stackaddr$prim54690 = alloca %struct.ScmObj*, align 8
%_95k47351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53984)
store volatile %struct.ScmObj* %_95k47351, %struct.ScmObj** %stackaddr$prim54690, align 8
%stackaddr$prim54691 = alloca %struct.ScmObj*, align 8
%current_45args53985 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53984)
store volatile %struct.ScmObj* %current_45args53985, %struct.ScmObj** %stackaddr$prim54691, align 8
%stackaddr$prim54692 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53985)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim54692, align 8
%stackaddr$makeclosure54693 = alloca %struct.ScmObj*, align 8
%fptrToInt54694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48084 to i64
%ae48084 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54694)
store volatile %struct.ScmObj* %ae48084, %struct.ScmObj** %stackaddr$makeclosure54693, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48084, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48084, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48084, %struct.ScmObj* %Ycmb47069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48084, %struct.ScmObj* %_37take47082, i64 3)
%argslist54484$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54695 = alloca %struct.ScmObj*, align 8
%argslist54484$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47233, %struct.ScmObj* %argslist54484$Ycmb470690)
store volatile %struct.ScmObj* %argslist54484$Ycmb470691, %struct.ScmObj** %stackaddr$prim54695, align 8
%stackaddr$prim54696 = alloca %struct.ScmObj*, align 8
%argslist54484$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48084, %struct.ScmObj* %argslist54484$Ycmb470691)
store volatile %struct.ScmObj* %argslist54484$Ycmb470692, %struct.ScmObj** %stackaddr$prim54696, align 8
%clofunc54697 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc54697(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist54484$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae48084(%struct.ScmObj* %env$ae48084,%struct.ScmObj* %current_45args53987) {
%stackaddr$env-ref54698 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48084, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54698
%stackaddr$env-ref54699 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48084, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54699
%stackaddr$env-ref54700 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48084, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54700
%stackaddr$env-ref54701 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48084, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref54701
%stackaddr$prim54702 = alloca %struct.ScmObj*, align 8
%_95k47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53987)
store volatile %struct.ScmObj* %_95k47352, %struct.ScmObj** %stackaddr$prim54702, align 8
%stackaddr$prim54703 = alloca %struct.ScmObj*, align 8
%current_45args53988 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53987)
store volatile %struct.ScmObj* %current_45args53988, %struct.ScmObj** %stackaddr$prim54703, align 8
%stackaddr$prim54704 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53988)
store volatile %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$prim54704, align 8
%stackaddr$makeclosure54705 = alloca %struct.ScmObj*, align 8
%fptrToInt54706 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48086 to i64
%ae48086 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54706)
store volatile %struct.ScmObj* %ae48086, %struct.ScmObj** %stackaddr$makeclosure54705, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %Ycmb47069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %_37take47082, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %_37length47079, i64 4)
%ae48087 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54707 = alloca %struct.ScmObj*, align 8
%fptrToInt54708 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48088 to i64
%ae48088 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54708)
store volatile %struct.ScmObj* %ae48088, %struct.ScmObj** %stackaddr$makeclosure54707, align 8
%argslist54483$ae480860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54709 = alloca %struct.ScmObj*, align 8
%argslist54483$ae480861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48088, %struct.ScmObj* %argslist54483$ae480860)
store volatile %struct.ScmObj* %argslist54483$ae480861, %struct.ScmObj** %stackaddr$prim54709, align 8
%stackaddr$prim54710 = alloca %struct.ScmObj*, align 8
%argslist54483$ae480862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48087, %struct.ScmObj* %argslist54483$ae480861)
store volatile %struct.ScmObj* %argslist54483$ae480862, %struct.ScmObj** %stackaddr$prim54710, align 8
%clofunc54711 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48086)
musttail call tailcc void %clofunc54711(%struct.ScmObj* %ae48086, %struct.ScmObj* %argslist54483$ae480862)
ret void
}

define tailcc void @proc_clo$ae48086(%struct.ScmObj* %env$ae48086,%struct.ScmObj* %current_45args53990) {
%stackaddr$env-ref54712 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54712
%stackaddr$env-ref54713 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54713
%stackaddr$env-ref54714 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54714
%stackaddr$env-ref54715 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref54715
%stackaddr$env-ref54716 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 4)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref54716
%stackaddr$prim54717 = alloca %struct.ScmObj*, align 8
%_95k47353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53990)
store volatile %struct.ScmObj* %_95k47353, %struct.ScmObj** %stackaddr$prim54717, align 8
%stackaddr$prim54718 = alloca %struct.ScmObj*, align 8
%current_45args53991 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53990)
store volatile %struct.ScmObj* %current_45args53991, %struct.ScmObj** %stackaddr$prim54718, align 8
%stackaddr$prim54719 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53991)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim54719, align 8
%stackaddr$makeclosure54720 = alloca %struct.ScmObj*, align 8
%fptrToInt54721 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48163 to i64
%ae48163 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54721)
store volatile %struct.ScmObj* %ae48163, %struct.ScmObj** %stackaddr$makeclosure54720, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48163, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48163, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48163, %struct.ScmObj* %Ycmb47069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48163, %struct.ScmObj* %_37take47082, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48163, %struct.ScmObj* %_37length47079, i64 4)
%argslist54467$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54722 = alloca %struct.ScmObj*, align 8
%argslist54467$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47238, %struct.ScmObj* %argslist54467$Ycmb470690)
store volatile %struct.ScmObj* %argslist54467$Ycmb470691, %struct.ScmObj** %stackaddr$prim54722, align 8
%stackaddr$prim54723 = alloca %struct.ScmObj*, align 8
%argslist54467$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48163, %struct.ScmObj* %argslist54467$Ycmb470691)
store volatile %struct.ScmObj* %argslist54467$Ycmb470692, %struct.ScmObj** %stackaddr$prim54723, align 8
%clofunc54724 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc54724(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist54467$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae48163(%struct.ScmObj* %env$ae48163,%struct.ScmObj* %current_45args53993) {
%stackaddr$env-ref54725 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48163, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54725
%stackaddr$env-ref54726 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48163, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54726
%stackaddr$env-ref54727 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48163, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54727
%stackaddr$env-ref54728 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48163, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref54728
%stackaddr$env-ref54729 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48163, i64 4)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref54729
%stackaddr$prim54730 = alloca %struct.ScmObj*, align 8
%_95k47354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53993)
store volatile %struct.ScmObj* %_95k47354, %struct.ScmObj** %stackaddr$prim54730, align 8
%stackaddr$prim54731 = alloca %struct.ScmObj*, align 8
%current_45args53994 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53993)
store volatile %struct.ScmObj* %current_45args53994, %struct.ScmObj** %stackaddr$prim54731, align 8
%stackaddr$prim54732 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53994)
store volatile %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$prim54732, align 8
%stackaddr$makeclosure54733 = alloca %struct.ScmObj*, align 8
%fptrToInt54734 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48165 to i64
%ae48165 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54734)
store volatile %struct.ScmObj* %ae48165, %struct.ScmObj** %stackaddr$makeclosure54733, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48165, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48165, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48165, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48165, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48165, %struct.ScmObj* %_37take47082, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48165, %struct.ScmObj* %_37length47079, i64 5)
%ae48166 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54735 = alloca %struct.ScmObj*, align 8
%fptrToInt54736 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48167 to i64
%ae48167 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54736)
store volatile %struct.ScmObj* %ae48167, %struct.ScmObj** %stackaddr$makeclosure54735, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48167, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist54466$ae481650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54737 = alloca %struct.ScmObj*, align 8
%argslist54466$ae481651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48167, %struct.ScmObj* %argslist54466$ae481650)
store volatile %struct.ScmObj* %argslist54466$ae481651, %struct.ScmObj** %stackaddr$prim54737, align 8
%stackaddr$prim54738 = alloca %struct.ScmObj*, align 8
%argslist54466$ae481652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48166, %struct.ScmObj* %argslist54466$ae481651)
store volatile %struct.ScmObj* %argslist54466$ae481652, %struct.ScmObj** %stackaddr$prim54738, align 8
%clofunc54739 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48165)
musttail call tailcc void %clofunc54739(%struct.ScmObj* %ae48165, %struct.ScmObj* %argslist54466$ae481652)
ret void
}

define tailcc void @proc_clo$ae48165(%struct.ScmObj* %env$ae48165,%struct.ScmObj* %current_45args53996) {
%stackaddr$env-ref54740 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48165, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54740
%stackaddr$env-ref54741 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48165, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54741
%stackaddr$env-ref54742 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48165, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54742
%stackaddr$env-ref54743 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48165, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54743
%stackaddr$env-ref54744 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48165, i64 4)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref54744
%stackaddr$env-ref54745 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48165, i64 5)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref54745
%stackaddr$prim54746 = alloca %struct.ScmObj*, align 8
%_95k47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53996)
store volatile %struct.ScmObj* %_95k47355, %struct.ScmObj** %stackaddr$prim54746, align 8
%stackaddr$prim54747 = alloca %struct.ScmObj*, align 8
%current_45args53997 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53996)
store volatile %struct.ScmObj* %current_45args53997, %struct.ScmObj** %stackaddr$prim54747, align 8
%stackaddr$prim54748 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53997)
store volatile %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$prim54748, align 8
%stackaddr$makeclosure54749 = alloca %struct.ScmObj*, align 8
%fptrToInt54750 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48219 to i64
%ae48219 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54750)
store volatile %struct.ScmObj* %ae48219, %struct.ScmObj** %stackaddr$makeclosure54749, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48219, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48219, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48219, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48219, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48219, %struct.ScmObj* %_37last47112, i64 4)
%ae48220 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54751 = alloca %struct.ScmObj*, align 8
%fptrToInt54752 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48221 to i64
%ae48221 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54752)
store volatile %struct.ScmObj* %ae48221, %struct.ScmObj** %stackaddr$makeclosure54751, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48221, %struct.ScmObj* %_37take47082, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48221, %struct.ScmObj* %_37length47079, i64 1)
%argslist54452$ae482190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54753 = alloca %struct.ScmObj*, align 8
%argslist54452$ae482191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48221, %struct.ScmObj* %argslist54452$ae482190)
store volatile %struct.ScmObj* %argslist54452$ae482191, %struct.ScmObj** %stackaddr$prim54753, align 8
%stackaddr$prim54754 = alloca %struct.ScmObj*, align 8
%argslist54452$ae482192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48220, %struct.ScmObj* %argslist54452$ae482191)
store volatile %struct.ScmObj* %argslist54452$ae482192, %struct.ScmObj** %stackaddr$prim54754, align 8
%clofunc54755 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48219)
musttail call tailcc void %clofunc54755(%struct.ScmObj* %ae48219, %struct.ScmObj* %argslist54452$ae482192)
ret void
}

define tailcc void @proc_clo$ae48219(%struct.ScmObj* %env$ae48219,%struct.ScmObj* %current_45args53999) {
%stackaddr$env-ref54756 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48219, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54756
%stackaddr$env-ref54757 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48219, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54757
%stackaddr$env-ref54758 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48219, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54758
%stackaddr$env-ref54759 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48219, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54759
%stackaddr$env-ref54760 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48219, i64 4)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref54760
%stackaddr$prim54761 = alloca %struct.ScmObj*, align 8
%_95k47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53999)
store volatile %struct.ScmObj* %_95k47356, %struct.ScmObj** %stackaddr$prim54761, align 8
%stackaddr$prim54762 = alloca %struct.ScmObj*, align 8
%current_45args54000 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53999)
store volatile %struct.ScmObj* %current_45args54000, %struct.ScmObj** %stackaddr$prim54762, align 8
%stackaddr$prim54763 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54000)
store volatile %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$prim54763, align 8
%stackaddr$makeclosure54764 = alloca %struct.ScmObj*, align 8
%fptrToInt54765 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48249 to i64
%ae48249 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54765)
store volatile %struct.ScmObj* %ae48249, %struct.ScmObj** %stackaddr$makeclosure54764, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48249, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48249, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48249, %struct.ScmObj* %_37drop_45right47109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48249, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48249, %struct.ScmObj* %_37last47112, i64 4)
%ae48250 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54766 = alloca %struct.ScmObj*, align 8
%fptrToInt54767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48251 to i64
%ae48251 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54767)
store volatile %struct.ScmObj* %ae48251, %struct.ScmObj** %stackaddr$makeclosure54766, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48251, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48251, %struct.ScmObj* %_37map147086, i64 1)
%argslist54442$ae482490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54768 = alloca %struct.ScmObj*, align 8
%argslist54442$ae482491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48251, %struct.ScmObj* %argslist54442$ae482490)
store volatile %struct.ScmObj* %argslist54442$ae482491, %struct.ScmObj** %stackaddr$prim54768, align 8
%stackaddr$prim54769 = alloca %struct.ScmObj*, align 8
%argslist54442$ae482492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48250, %struct.ScmObj* %argslist54442$ae482491)
store volatile %struct.ScmObj* %argslist54442$ae482492, %struct.ScmObj** %stackaddr$prim54769, align 8
%clofunc54770 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48249)
musttail call tailcc void %clofunc54770(%struct.ScmObj* %ae48249, %struct.ScmObj* %argslist54442$ae482492)
ret void
}

define tailcc void @proc_clo$ae48249(%struct.ScmObj* %env$ae48249,%struct.ScmObj* %current_45args54002) {
%stackaddr$env-ref54771 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48249, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54771
%stackaddr$env-ref54772 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48249, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54772
%stackaddr$env-ref54773 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48249, i64 2)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref54773
%stackaddr$env-ref54774 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48249, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54774
%stackaddr$env-ref54775 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48249, i64 4)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref54775
%stackaddr$prim54776 = alloca %struct.ScmObj*, align 8
%_95k47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54002)
store volatile %struct.ScmObj* %_95k47357, %struct.ScmObj** %stackaddr$prim54776, align 8
%stackaddr$prim54777 = alloca %struct.ScmObj*, align 8
%current_45args54003 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54002)
store volatile %struct.ScmObj* %current_45args54003, %struct.ScmObj** %stackaddr$prim54777, align 8
%stackaddr$prim54778 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54003)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim54778, align 8
%stackaddr$makeclosure54779 = alloca %struct.ScmObj*, align 8
%fptrToInt54780 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48633 to i64
%ae48633 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54780)
store volatile %struct.ScmObj* %ae48633, %struct.ScmObj** %stackaddr$makeclosure54779, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %_37drop_45right47109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %_37last47112, i64 4)
%argslist54382$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54781 = alloca %struct.ScmObj*, align 8
%argslist54382$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %argslist54382$Ycmb470690)
store volatile %struct.ScmObj* %argslist54382$Ycmb470691, %struct.ScmObj** %stackaddr$prim54781, align 8
%stackaddr$prim54782 = alloca %struct.ScmObj*, align 8
%argslist54382$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48633, %struct.ScmObj* %argslist54382$Ycmb470691)
store volatile %struct.ScmObj* %argslist54382$Ycmb470692, %struct.ScmObj** %stackaddr$prim54782, align 8
%clofunc54783 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc54783(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist54382$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae48633(%struct.ScmObj* %env$ae48633,%struct.ScmObj* %current_45args54005) {
%stackaddr$env-ref54784 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54784
%stackaddr$env-ref54785 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54785
%stackaddr$env-ref54786 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 2)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref54786
%stackaddr$env-ref54787 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54787
%stackaddr$env-ref54788 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 4)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref54788
%stackaddr$prim54789 = alloca %struct.ScmObj*, align 8
%_95k47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54005)
store volatile %struct.ScmObj* %_95k47358, %struct.ScmObj** %stackaddr$prim54789, align 8
%stackaddr$prim54790 = alloca %struct.ScmObj*, align 8
%current_45args54006 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54005)
store volatile %struct.ScmObj* %current_45args54006, %struct.ScmObj** %stackaddr$prim54790, align 8
%stackaddr$prim54791 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54006)
store volatile %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$prim54791, align 8
%stackaddr$makeclosure54792 = alloca %struct.ScmObj*, align 8
%fptrToInt54793 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48635 to i64
%ae48635 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54793)
store volatile %struct.ScmObj* %ae48635, %struct.ScmObj** %stackaddr$makeclosure54792, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %_37drop_45right47109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %_37last47112, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %_37foldr47095, i64 5)
%ae48636 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54794 = alloca %struct.ScmObj*, align 8
%fptrToInt54795 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48637 to i64
%ae48637 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54795)
store volatile %struct.ScmObj* %ae48637, %struct.ScmObj** %stackaddr$makeclosure54794, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48637, %struct.ScmObj* %_37foldr147090, i64 0)
%argslist54381$ae486350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54796 = alloca %struct.ScmObj*, align 8
%argslist54381$ae486351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48637, %struct.ScmObj* %argslist54381$ae486350)
store volatile %struct.ScmObj* %argslist54381$ae486351, %struct.ScmObj** %stackaddr$prim54796, align 8
%stackaddr$prim54797 = alloca %struct.ScmObj*, align 8
%argslist54381$ae486352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48636, %struct.ScmObj* %argslist54381$ae486351)
store volatile %struct.ScmObj* %argslist54381$ae486352, %struct.ScmObj** %stackaddr$prim54797, align 8
%clofunc54798 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48635)
musttail call tailcc void %clofunc54798(%struct.ScmObj* %ae48635, %struct.ScmObj* %argslist54381$ae486352)
ret void
}

define tailcc void @proc_clo$ae48635(%struct.ScmObj* %env$ae48635,%struct.ScmObj* %current_45args54008) {
%stackaddr$env-ref54799 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54799
%stackaddr$env-ref54800 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54800
%stackaddr$env-ref54801 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 2)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref54801
%stackaddr$env-ref54802 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54802
%stackaddr$env-ref54803 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 4)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref54803
%stackaddr$env-ref54804 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 5)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54804
%stackaddr$prim54805 = alloca %struct.ScmObj*, align 8
%_95k47359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54008)
store volatile %struct.ScmObj* %_95k47359, %struct.ScmObj** %stackaddr$prim54805, align 8
%stackaddr$prim54806 = alloca %struct.ScmObj*, align 8
%current_45args54009 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54008)
store volatile %struct.ScmObj* %current_45args54009, %struct.ScmObj** %stackaddr$prim54806, align 8
%stackaddr$prim54807 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54009)
store volatile %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$prim54807, align 8
%stackaddr$makeclosure54808 = alloca %struct.ScmObj*, align 8
%fptrToInt54809 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48712 to i64
%ae48712 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54809)
store volatile %struct.ScmObj* %ae48712, %struct.ScmObj** %stackaddr$makeclosure54808, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48712, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48712, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48712, %struct.ScmObj* %_37map147121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48712, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48712, %struct.ScmObj* %_37foldr47095, i64 4)
%ae48713 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54810 = alloca %struct.ScmObj*, align 8
%fptrToInt54811 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48714 to i64
%ae48714 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54811)
store volatile %struct.ScmObj* %ae48714, %struct.ScmObj** %stackaddr$makeclosure54810, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48714, %struct.ScmObj* %_37drop_45right47109, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48714, %struct.ScmObj* %_37last47112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48714, %struct.ScmObj* %_37foldr47095, i64 2)
%argslist54362$ae487120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54812 = alloca %struct.ScmObj*, align 8
%argslist54362$ae487121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48714, %struct.ScmObj* %argslist54362$ae487120)
store volatile %struct.ScmObj* %argslist54362$ae487121, %struct.ScmObj** %stackaddr$prim54812, align 8
%stackaddr$prim54813 = alloca %struct.ScmObj*, align 8
%argslist54362$ae487122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48713, %struct.ScmObj* %argslist54362$ae487121)
store volatile %struct.ScmObj* %argslist54362$ae487122, %struct.ScmObj** %stackaddr$prim54813, align 8
%clofunc54814 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48712)
musttail call tailcc void %clofunc54814(%struct.ScmObj* %ae48712, %struct.ScmObj* %argslist54362$ae487122)
ret void
}

define tailcc void @proc_clo$ae48712(%struct.ScmObj* %env$ae48712,%struct.ScmObj* %current_45args54011) {
%stackaddr$env-ref54815 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48712, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54815
%stackaddr$env-ref54816 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48712, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54816
%stackaddr$env-ref54817 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48712, i64 2)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54817
%stackaddr$env-ref54818 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48712, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54818
%stackaddr$env-ref54819 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48712, i64 4)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54819
%stackaddr$prim54820 = alloca %struct.ScmObj*, align 8
%_95k47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54011)
store volatile %struct.ScmObj* %_95k47360, %struct.ScmObj** %stackaddr$prim54820, align 8
%stackaddr$prim54821 = alloca %struct.ScmObj*, align 8
%current_45args54012 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54011)
store volatile %struct.ScmObj* %current_45args54012, %struct.ScmObj** %stackaddr$prim54821, align 8
%stackaddr$prim54822 = alloca %struct.ScmObj*, align 8
%_37map47116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54012)
store volatile %struct.ScmObj* %_37map47116, %struct.ScmObj** %stackaddr$prim54822, align 8
%stackaddr$makeclosure54823 = alloca %struct.ScmObj*, align 8
%fptrToInt54824 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48858 to i64
%ae48858 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54824)
store volatile %struct.ScmObj* %ae48858, %struct.ScmObj** %stackaddr$makeclosure54823, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48858, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48858, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48858, %struct.ScmObj* %Ycmb47069, i64 2)
%ae48859 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54825 = alloca %struct.ScmObj*, align 8
%fptrToInt54826 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48860 to i64
%ae48860 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54826)
store volatile %struct.ScmObj* %ae48860, %struct.ScmObj** %stackaddr$makeclosure54825, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48860, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48860, %struct.ScmObj* %_37map147121, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48860, %struct.ScmObj* %_37foldr47095, i64 2)
%argslist54345$ae488580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54827 = alloca %struct.ScmObj*, align 8
%argslist54345$ae488581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48860, %struct.ScmObj* %argslist54345$ae488580)
store volatile %struct.ScmObj* %argslist54345$ae488581, %struct.ScmObj** %stackaddr$prim54827, align 8
%stackaddr$prim54828 = alloca %struct.ScmObj*, align 8
%argslist54345$ae488582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48859, %struct.ScmObj* %argslist54345$ae488581)
store volatile %struct.ScmObj* %argslist54345$ae488582, %struct.ScmObj** %stackaddr$prim54828, align 8
%clofunc54829 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48858)
musttail call tailcc void %clofunc54829(%struct.ScmObj* %ae48858, %struct.ScmObj* %argslist54345$ae488582)
ret void
}

define tailcc void @proc_clo$ae48858(%struct.ScmObj* %env$ae48858,%struct.ScmObj* %current_45args54014) {
%stackaddr$env-ref54830 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48858, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54830
%stackaddr$env-ref54831 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48858, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54831
%stackaddr$env-ref54832 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48858, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref54832
%stackaddr$prim54833 = alloca %struct.ScmObj*, align 8
%_95k47361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54014)
store volatile %struct.ScmObj* %_95k47361, %struct.ScmObj** %stackaddr$prim54833, align 8
%stackaddr$prim54834 = alloca %struct.ScmObj*, align 8
%current_45args54015 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54014)
store volatile %struct.ScmObj* %current_45args54015, %struct.ScmObj** %stackaddr$prim54834, align 8
%stackaddr$prim54835 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54015)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54835, align 8
%stackaddr$makeclosure54836 = alloca %struct.ScmObj*, align 8
%fptrToInt54837 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49250 to i64
%ae49250 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54837)
store volatile %struct.ScmObj* %ae49250, %struct.ScmObj** %stackaddr$makeclosure54836, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49250, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49250, %struct.ScmObj* %_37foldl147074, i64 1)
%argslist54285$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54838 = alloca %struct.ScmObj*, align 8
%argslist54285$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47274, %struct.ScmObj* %argslist54285$Ycmb470690)
store volatile %struct.ScmObj* %argslist54285$Ycmb470691, %struct.ScmObj** %stackaddr$prim54838, align 8
%stackaddr$prim54839 = alloca %struct.ScmObj*, align 8
%argslist54285$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49250, %struct.ScmObj* %argslist54285$Ycmb470691)
store volatile %struct.ScmObj* %argslist54285$Ycmb470692, %struct.ScmObj** %stackaddr$prim54839, align 8
%clofunc54840 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc54840(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist54285$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae49250(%struct.ScmObj* %env$ae49250,%struct.ScmObj* %current_45args54017) {
%stackaddr$env-ref54841 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49250, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54841
%stackaddr$env-ref54842 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49250, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54842
%stackaddr$prim54843 = alloca %struct.ScmObj*, align 8
%_95k47362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54017)
store volatile %struct.ScmObj* %_95k47362, %struct.ScmObj** %stackaddr$prim54843, align 8
%stackaddr$prim54844 = alloca %struct.ScmObj*, align 8
%current_45args54018 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54017)
store volatile %struct.ScmObj* %current_45args54018, %struct.ScmObj** %stackaddr$prim54844, align 8
%stackaddr$prim54845 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54018)
store volatile %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$prim54845, align 8
%stackaddr$makeclosure54846 = alloca %struct.ScmObj*, align 8
%fptrToInt54847 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49252 to i64
%ae49252 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54847)
store volatile %struct.ScmObj* %ae49252, %struct.ScmObj** %stackaddr$makeclosure54846, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %_37foldl47172, i64 2)
%ae49253 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54848 = alloca %struct.ScmObj*, align 8
%fptrToInt54849 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49254 to i64
%ae49254 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54849)
store volatile %struct.ScmObj* %ae49254, %struct.ScmObj** %stackaddr$makeclosure54848, align 8
%argslist54284$ae492520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54850 = alloca %struct.ScmObj*, align 8
%argslist54284$ae492521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49254, %struct.ScmObj* %argslist54284$ae492520)
store volatile %struct.ScmObj* %argslist54284$ae492521, %struct.ScmObj** %stackaddr$prim54850, align 8
%stackaddr$prim54851 = alloca %struct.ScmObj*, align 8
%argslist54284$ae492522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49253, %struct.ScmObj* %argslist54284$ae492521)
store volatile %struct.ScmObj* %argslist54284$ae492522, %struct.ScmObj** %stackaddr$prim54851, align 8
%clofunc54852 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49252)
musttail call tailcc void %clofunc54852(%struct.ScmObj* %ae49252, %struct.ScmObj* %argslist54284$ae492522)
ret void
}

define tailcc void @proc_clo$ae49252(%struct.ScmObj* %env$ae49252,%struct.ScmObj* %current_45args54020) {
%stackaddr$env-ref54853 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54853
%stackaddr$env-ref54854 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54854
%stackaddr$env-ref54855 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54855
%stackaddr$prim54856 = alloca %struct.ScmObj*, align 8
%_95k47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54020)
store volatile %struct.ScmObj* %_95k47363, %struct.ScmObj** %stackaddr$prim54856, align 8
%stackaddr$prim54857 = alloca %struct.ScmObj*, align 8
%current_45args54021 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54020)
store volatile %struct.ScmObj* %current_45args54021, %struct.ScmObj** %stackaddr$prim54857, align 8
%stackaddr$prim54858 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54021)
store volatile %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$prim54858, align 8
%stackaddr$makeclosure54859 = alloca %struct.ScmObj*, align 8
%fptrToInt54860 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49276 to i64
%ae49276 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54860)
store volatile %struct.ScmObj* %ae49276, %struct.ScmObj** %stackaddr$makeclosure54859, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %_37foldl47172, i64 2)
%ae49277 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54861 = alloca %struct.ScmObj*, align 8
%fptrToInt54862 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49278 to i64
%ae49278 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54862)
store volatile %struct.ScmObj* %ae49278, %struct.ScmObj** %stackaddr$makeclosure54861, align 8
%argslist54278$ae492760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54863 = alloca %struct.ScmObj*, align 8
%argslist54278$ae492761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49278, %struct.ScmObj* %argslist54278$ae492760)
store volatile %struct.ScmObj* %argslist54278$ae492761, %struct.ScmObj** %stackaddr$prim54863, align 8
%stackaddr$prim54864 = alloca %struct.ScmObj*, align 8
%argslist54278$ae492762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49277, %struct.ScmObj* %argslist54278$ae492761)
store volatile %struct.ScmObj* %argslist54278$ae492762, %struct.ScmObj** %stackaddr$prim54864, align 8
%clofunc54865 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49276)
musttail call tailcc void %clofunc54865(%struct.ScmObj* %ae49276, %struct.ScmObj* %argslist54278$ae492762)
ret void
}

define tailcc void @proc_clo$ae49276(%struct.ScmObj* %env$ae49276,%struct.ScmObj* %current_45args54023) {
%stackaddr$env-ref54866 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54866
%stackaddr$env-ref54867 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54867
%stackaddr$env-ref54868 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54868
%stackaddr$prim54869 = alloca %struct.ScmObj*, align 8
%_95k47364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54023)
store volatile %struct.ScmObj* %_95k47364, %struct.ScmObj** %stackaddr$prim54869, align 8
%stackaddr$prim54870 = alloca %struct.ScmObj*, align 8
%current_45args54024 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54023)
store volatile %struct.ScmObj* %current_45args54024, %struct.ScmObj** %stackaddr$prim54870, align 8
%stackaddr$prim54871 = alloca %struct.ScmObj*, align 8
%_37_62_6147166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54024)
store volatile %struct.ScmObj* %_37_62_6147166, %struct.ScmObj** %stackaddr$prim54871, align 8
%ae49300 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49301 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54872 = alloca %struct.ScmObj*, align 8
%_37append47162 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49300, %struct.ScmObj* %ae49301)
store volatile %struct.ScmObj* %_37append47162, %struct.ScmObj** %stackaddr$prim54872, align 8
%stackaddr$makeclosure54873 = alloca %struct.ScmObj*, align 8
%fptrToInt54874 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49302 to i64
%ae49302 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54874)
store volatile %struct.ScmObj* %ae49302, %struct.ScmObj** %stackaddr$makeclosure54873, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49302, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49302, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49302, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49302, %struct.ScmObj* %_37append47162, i64 3)
%ae49303 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54875 = alloca %struct.ScmObj*, align 8
%fptrToInt54876 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49304 to i64
%ae49304 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54876)
store volatile %struct.ScmObj* %ae49304, %struct.ScmObj** %stackaddr$makeclosure54875, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49304, %struct.ScmObj* %_37append47162, i64 0)
%argslist54272$ae493020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54877 = alloca %struct.ScmObj*, align 8
%argslist54272$ae493021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49304, %struct.ScmObj* %argslist54272$ae493020)
store volatile %struct.ScmObj* %argslist54272$ae493021, %struct.ScmObj** %stackaddr$prim54877, align 8
%stackaddr$prim54878 = alloca %struct.ScmObj*, align 8
%argslist54272$ae493022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49303, %struct.ScmObj* %argslist54272$ae493021)
store volatile %struct.ScmObj* %argslist54272$ae493022, %struct.ScmObj** %stackaddr$prim54878, align 8
%clofunc54879 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49302)
musttail call tailcc void %clofunc54879(%struct.ScmObj* %ae49302, %struct.ScmObj* %argslist54272$ae493022)
ret void
}

define tailcc void @proc_clo$ae49302(%struct.ScmObj* %env$ae49302,%struct.ScmObj* %current_45args54026) {
%stackaddr$env-ref54880 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49302, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54880
%stackaddr$env-ref54881 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49302, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54881
%stackaddr$env-ref54882 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49302, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54882
%stackaddr$env-ref54883 = alloca %struct.ScmObj*, align 8
%_37append47162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49302, i64 3)
store %struct.ScmObj* %_37append47162, %struct.ScmObj** %stackaddr$env-ref54883
%stackaddr$prim54884 = alloca %struct.ScmObj*, align 8
%_95k47365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54026)
store volatile %struct.ScmObj* %_95k47365, %struct.ScmObj** %stackaddr$prim54884, align 8
%stackaddr$prim54885 = alloca %struct.ScmObj*, align 8
%current_45args54027 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54026)
store volatile %struct.ScmObj* %current_45args54027, %struct.ScmObj** %stackaddr$prim54885, align 8
%stackaddr$prim54886 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54027)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim54886, align 8
%ae49370 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54887 = alloca %struct.ScmObj*, align 8
%_95047163 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47162, %struct.ScmObj* %ae49370, %struct.ScmObj* %anf_45bind47282)
store volatile %struct.ScmObj* %_95047163, %struct.ScmObj** %stackaddr$prim54887, align 8
%ae49373 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54888 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47162, %struct.ScmObj* %ae49373)
store volatile %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$prim54888, align 8
%stackaddr$makeclosure54889 = alloca %struct.ScmObj*, align 8
%fptrToInt54890 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49374 to i64
%ae49374 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54890)
store volatile %struct.ScmObj* %ae49374, %struct.ScmObj** %stackaddr$makeclosure54889, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49374, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49374, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49374, %struct.ScmObj* %_37foldl47172, i64 2)
%ae49375 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54891 = alloca %struct.ScmObj*, align 8
%fptrToInt54892 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49376 to i64
%ae49376 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54892)
store volatile %struct.ScmObj* %ae49376, %struct.ScmObj** %stackaddr$makeclosure54891, align 8
%argslist54261$ae493740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54893 = alloca %struct.ScmObj*, align 8
%argslist54261$ae493741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49376, %struct.ScmObj* %argslist54261$ae493740)
store volatile %struct.ScmObj* %argslist54261$ae493741, %struct.ScmObj** %stackaddr$prim54893, align 8
%stackaddr$prim54894 = alloca %struct.ScmObj*, align 8
%argslist54261$ae493742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49375, %struct.ScmObj* %argslist54261$ae493741)
store volatile %struct.ScmObj* %argslist54261$ae493742, %struct.ScmObj** %stackaddr$prim54894, align 8
%clofunc54895 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49374)
musttail call tailcc void %clofunc54895(%struct.ScmObj* %ae49374, %struct.ScmObj* %argslist54261$ae493742)
ret void
}

define tailcc void @proc_clo$ae49374(%struct.ScmObj* %env$ae49374,%struct.ScmObj* %current_45args54029) {
%stackaddr$env-ref54896 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49374, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54896
%stackaddr$env-ref54897 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49374, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54897
%stackaddr$env-ref54898 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49374, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54898
%stackaddr$prim54899 = alloca %struct.ScmObj*, align 8
%_95k47366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54029)
store volatile %struct.ScmObj* %_95k47366, %struct.ScmObj** %stackaddr$prim54899, align 8
%stackaddr$prim54900 = alloca %struct.ScmObj*, align 8
%current_45args54030 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54029)
store volatile %struct.ScmObj* %current_45args54030, %struct.ScmObj** %stackaddr$prim54900, align 8
%stackaddr$prim54901 = alloca %struct.ScmObj*, align 8
%_37list_6347154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54030)
store volatile %struct.ScmObj* %_37list_6347154, %struct.ScmObj** %stackaddr$prim54901, align 8
%stackaddr$makeclosure54902 = alloca %struct.ScmObj*, align 8
%fptrToInt54903 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49790 to i64
%ae49790 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54903)
store volatile %struct.ScmObj* %ae49790, %struct.ScmObj** %stackaddr$makeclosure54902, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49790, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49790, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49790, %struct.ScmObj* %_37foldl47172, i64 2)
%ae49791 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54904 = alloca %struct.ScmObj*, align 8
%fptrToInt54905 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49792 to i64
%ae49792 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54905)
store volatile %struct.ScmObj* %ae49792, %struct.ScmObj** %stackaddr$makeclosure54904, align 8
%argslist54236$ae497900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54906 = alloca %struct.ScmObj*, align 8
%argslist54236$ae497901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49792, %struct.ScmObj* %argslist54236$ae497900)
store volatile %struct.ScmObj* %argslist54236$ae497901, %struct.ScmObj** %stackaddr$prim54906, align 8
%stackaddr$prim54907 = alloca %struct.ScmObj*, align 8
%argslist54236$ae497902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49791, %struct.ScmObj* %argslist54236$ae497901)
store volatile %struct.ScmObj* %argslist54236$ae497902, %struct.ScmObj** %stackaddr$prim54907, align 8
%clofunc54908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49790)
musttail call tailcc void %clofunc54908(%struct.ScmObj* %ae49790, %struct.ScmObj* %argslist54236$ae497902)
ret void
}

define tailcc void @proc_clo$ae49790(%struct.ScmObj* %env$ae49790,%struct.ScmObj* %current_45args54032) {
%stackaddr$env-ref54909 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49790, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54909
%stackaddr$env-ref54910 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49790, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54910
%stackaddr$env-ref54911 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49790, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54911
%stackaddr$prim54912 = alloca %struct.ScmObj*, align 8
%_95k47367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54032)
store volatile %struct.ScmObj* %_95k47367, %struct.ScmObj** %stackaddr$prim54912, align 8
%stackaddr$prim54913 = alloca %struct.ScmObj*, align 8
%current_45args54033 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54032)
store volatile %struct.ScmObj* %current_45args54033, %struct.ScmObj** %stackaddr$prim54913, align 8
%stackaddr$prim54914 = alloca %struct.ScmObj*, align 8
%_37drop47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54033)
store volatile %struct.ScmObj* %_37drop47145, %struct.ScmObj** %stackaddr$prim54914, align 8
%stackaddr$makeclosure54915 = alloca %struct.ScmObj*, align 8
%fptrToInt54916 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50326 to i64
%ae50326 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54916)
store volatile %struct.ScmObj* %ae50326, %struct.ScmObj** %stackaddr$makeclosure54915, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50326, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50326, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50326, %struct.ScmObj* %_37foldl47172, i64 2)
%ae50327 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54917 = alloca %struct.ScmObj*, align 8
%fptrToInt54918 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50328 to i64
%ae50328 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54918)
store volatile %struct.ScmObj* %ae50328, %struct.ScmObj** %stackaddr$makeclosure54917, align 8
%argslist54212$ae503260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54919 = alloca %struct.ScmObj*, align 8
%argslist54212$ae503261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50328, %struct.ScmObj* %argslist54212$ae503260)
store volatile %struct.ScmObj* %argslist54212$ae503261, %struct.ScmObj** %stackaddr$prim54919, align 8
%stackaddr$prim54920 = alloca %struct.ScmObj*, align 8
%argslist54212$ae503262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50327, %struct.ScmObj* %argslist54212$ae503261)
store volatile %struct.ScmObj* %argslist54212$ae503262, %struct.ScmObj** %stackaddr$prim54920, align 8
%clofunc54921 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50326)
musttail call tailcc void %clofunc54921(%struct.ScmObj* %ae50326, %struct.ScmObj* %argslist54212$ae503262)
ret void
}

define tailcc void @proc_clo$ae50326(%struct.ScmObj* %env$ae50326,%struct.ScmObj* %current_45args54035) {
%stackaddr$env-ref54922 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50326, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54922
%stackaddr$env-ref54923 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50326, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54923
%stackaddr$env-ref54924 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50326, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54924
%stackaddr$prim54925 = alloca %struct.ScmObj*, align 8
%_95k47368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54035)
store volatile %struct.ScmObj* %_95k47368, %struct.ScmObj** %stackaddr$prim54925, align 8
%stackaddr$prim54926 = alloca %struct.ScmObj*, align 8
%current_45args54036 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54035)
store volatile %struct.ScmObj* %current_45args54036, %struct.ScmObj** %stackaddr$prim54926, align 8
%stackaddr$prim54927 = alloca %struct.ScmObj*, align 8
%_37memv47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54036)
store volatile %struct.ScmObj* %_37memv47138, %struct.ScmObj** %stackaddr$prim54927, align 8
%stackaddr$makeclosure54928 = alloca %struct.ScmObj*, align 8
%fptrToInt54929 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50730 to i64
%ae50730 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54929)
store volatile %struct.ScmObj* %ae50730, %struct.ScmObj** %stackaddr$makeclosure54928, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50730, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50730, %struct.ScmObj* %_37foldr147090, i64 1)
%ae50731 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54930 = alloca %struct.ScmObj*, align 8
%fptrToInt54931 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50732 to i64
%ae50732 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54931)
store volatile %struct.ScmObj* %ae50732, %struct.ScmObj** %stackaddr$makeclosure54930, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50732, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist54186$ae507300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54932 = alloca %struct.ScmObj*, align 8
%argslist54186$ae507301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50732, %struct.ScmObj* %argslist54186$ae507300)
store volatile %struct.ScmObj* %argslist54186$ae507301, %struct.ScmObj** %stackaddr$prim54932, align 8
%stackaddr$prim54933 = alloca %struct.ScmObj*, align 8
%argslist54186$ae507302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50731, %struct.ScmObj* %argslist54186$ae507301)
store volatile %struct.ScmObj* %argslist54186$ae507302, %struct.ScmObj** %stackaddr$prim54933, align 8
%clofunc54934 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50730)
musttail call tailcc void %clofunc54934(%struct.ScmObj* %ae50730, %struct.ScmObj* %argslist54186$ae507302)
ret void
}

define tailcc void @proc_clo$ae50730(%struct.ScmObj* %env$ae50730,%struct.ScmObj* %current_45args54038) {
%stackaddr$env-ref54935 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50730, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54935
%stackaddr$env-ref54936 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50730, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54936
%stackaddr$prim54937 = alloca %struct.ScmObj*, align 8
%_95k47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54038)
store volatile %struct.ScmObj* %_95k47369, %struct.ScmObj** %stackaddr$prim54937, align 8
%stackaddr$prim54938 = alloca %struct.ScmObj*, align 8
%current_45args54039 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54038)
store volatile %struct.ScmObj* %current_45args54039, %struct.ScmObj** %stackaddr$prim54938, align 8
%stackaddr$prim54939 = alloca %struct.ScmObj*, align 8
%_37_4747134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54039)
store volatile %struct.ScmObj* %_37_4747134, %struct.ScmObj** %stackaddr$prim54939, align 8
%stackaddr$makeclosure54940 = alloca %struct.ScmObj*, align 8
%fptrToInt54941 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50828 to i64
%ae50828 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54941)
store volatile %struct.ScmObj* %ae50828, %struct.ScmObj** %stackaddr$makeclosure54940, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50828, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50828, %struct.ScmObj* %_37foldr147090, i64 1)
%ae50829 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54942 = alloca %struct.ScmObj*, align 8
%fptrToInt54943 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50830 to i64
%ae50830 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54943)
store volatile %struct.ScmObj* %ae50830, %struct.ScmObj** %stackaddr$makeclosure54942, align 8
%argslist54173$ae508280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54944 = alloca %struct.ScmObj*, align 8
%argslist54173$ae508281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50830, %struct.ScmObj* %argslist54173$ae508280)
store volatile %struct.ScmObj* %argslist54173$ae508281, %struct.ScmObj** %stackaddr$prim54944, align 8
%stackaddr$prim54945 = alloca %struct.ScmObj*, align 8
%argslist54173$ae508282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50829, %struct.ScmObj* %argslist54173$ae508281)
store volatile %struct.ScmObj* %argslist54173$ae508282, %struct.ScmObj** %stackaddr$prim54945, align 8
%clofunc54946 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50828)
musttail call tailcc void %clofunc54946(%struct.ScmObj* %ae50828, %struct.ScmObj* %argslist54173$ae508282)
ret void
}

define tailcc void @proc_clo$ae50828(%struct.ScmObj* %env$ae50828,%struct.ScmObj* %current_45args54041) {
%stackaddr$env-ref54947 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50828, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54947
%stackaddr$env-ref54948 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50828, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54948
%stackaddr$prim54949 = alloca %struct.ScmObj*, align 8
%_95k47370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54041)
store volatile %struct.ScmObj* %_95k47370, %struct.ScmObj** %stackaddr$prim54949, align 8
%stackaddr$prim54950 = alloca %struct.ScmObj*, align 8
%current_45args54042 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54041)
store volatile %struct.ScmObj* %current_45args54042, %struct.ScmObj** %stackaddr$prim54950, align 8
%stackaddr$prim54951 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54042)
store volatile %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$prim54951, align 8
%stackaddr$makeclosure54952 = alloca %struct.ScmObj*, align 8
%fptrToInt54953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50848 to i64
%ae50848 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54953)
store volatile %struct.ScmObj* %ae50848, %struct.ScmObj** %stackaddr$makeclosure54952, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50848, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50848, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50848, %struct.ScmObj* %_37first47132, i64 2)
%ae50849 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54954 = alloca %struct.ScmObj*, align 8
%fptrToInt54955 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50850 to i64
%ae50850 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54955)
store volatile %struct.ScmObj* %ae50850, %struct.ScmObj** %stackaddr$makeclosure54954, align 8
%argslist54168$ae508480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54956 = alloca %struct.ScmObj*, align 8
%argslist54168$ae508481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50850, %struct.ScmObj* %argslist54168$ae508480)
store volatile %struct.ScmObj* %argslist54168$ae508481, %struct.ScmObj** %stackaddr$prim54956, align 8
%stackaddr$prim54957 = alloca %struct.ScmObj*, align 8
%argslist54168$ae508482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50849, %struct.ScmObj* %argslist54168$ae508481)
store volatile %struct.ScmObj* %argslist54168$ae508482, %struct.ScmObj** %stackaddr$prim54957, align 8
%clofunc54958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50848)
musttail call tailcc void %clofunc54958(%struct.ScmObj* %ae50848, %struct.ScmObj* %argslist54168$ae508482)
ret void
}

define tailcc void @proc_clo$ae50848(%struct.ScmObj* %env$ae50848,%struct.ScmObj* %current_45args54044) {
%stackaddr$env-ref54959 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50848, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54959
%stackaddr$env-ref54960 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50848, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54960
%stackaddr$env-ref54961 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50848, i64 2)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref54961
%stackaddr$prim54962 = alloca %struct.ScmObj*, align 8
%_95k47371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54044)
store volatile %struct.ScmObj* %_95k47371, %struct.ScmObj** %stackaddr$prim54962, align 8
%stackaddr$prim54963 = alloca %struct.ScmObj*, align 8
%current_45args54045 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54044)
store volatile %struct.ScmObj* %current_45args54045, %struct.ScmObj** %stackaddr$prim54963, align 8
%stackaddr$prim54964 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54045)
store volatile %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$prim54964, align 8
%stackaddr$makeclosure54965 = alloca %struct.ScmObj*, align 8
%fptrToInt54966 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50870 to i64
%ae50870 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54966)
store volatile %struct.ScmObj* %ae50870, %struct.ScmObj** %stackaddr$makeclosure54965, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50870, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50870, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50870, %struct.ScmObj* %_37first47132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50870, %struct.ScmObj* %_37second47130, i64 3)
%ae50871 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54967 = alloca %struct.ScmObj*, align 8
%fptrToInt54968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50872 to i64
%ae50872 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54968)
store volatile %struct.ScmObj* %ae50872, %struct.ScmObj** %stackaddr$makeclosure54967, align 8
%argslist54163$ae508700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54969 = alloca %struct.ScmObj*, align 8
%argslist54163$ae508701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50872, %struct.ScmObj* %argslist54163$ae508700)
store volatile %struct.ScmObj* %argslist54163$ae508701, %struct.ScmObj** %stackaddr$prim54969, align 8
%stackaddr$prim54970 = alloca %struct.ScmObj*, align 8
%argslist54163$ae508702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50871, %struct.ScmObj* %argslist54163$ae508701)
store volatile %struct.ScmObj* %argslist54163$ae508702, %struct.ScmObj** %stackaddr$prim54970, align 8
%clofunc54971 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50870)
musttail call tailcc void %clofunc54971(%struct.ScmObj* %ae50870, %struct.ScmObj* %argslist54163$ae508702)
ret void
}

define tailcc void @proc_clo$ae50870(%struct.ScmObj* %env$ae50870,%struct.ScmObj* %current_45args54047) {
%stackaddr$env-ref54972 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50870, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54972
%stackaddr$env-ref54973 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50870, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54973
%stackaddr$env-ref54974 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50870, i64 2)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref54974
%stackaddr$env-ref54975 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50870, i64 3)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref54975
%stackaddr$prim54976 = alloca %struct.ScmObj*, align 8
%_95k47372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54047)
store volatile %struct.ScmObj* %_95k47372, %struct.ScmObj** %stackaddr$prim54976, align 8
%stackaddr$prim54977 = alloca %struct.ScmObj*, align 8
%current_45args54048 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54047)
store volatile %struct.ScmObj* %current_45args54048, %struct.ScmObj** %stackaddr$prim54977, align 8
%stackaddr$prim54978 = alloca %struct.ScmObj*, align 8
%_37third47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54048)
store volatile %struct.ScmObj* %_37third47128, %struct.ScmObj** %stackaddr$prim54978, align 8
%stackaddr$makeclosure54979 = alloca %struct.ScmObj*, align 8
%fptrToInt54980 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50894 to i64
%ae50894 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54980)
store volatile %struct.ScmObj* %ae50894, %struct.ScmObj** %stackaddr$makeclosure54979, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50894, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50894, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50894, %struct.ScmObj* %_37first47132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50894, %struct.ScmObj* %_37second47130, i64 3)
%ae50895 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54981 = alloca %struct.ScmObj*, align 8
%fptrToInt54982 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50896 to i64
%ae50896 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54982)
store volatile %struct.ScmObj* %ae50896, %struct.ScmObj** %stackaddr$makeclosure54981, align 8
%argslist54158$ae508940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54983 = alloca %struct.ScmObj*, align 8
%argslist54158$ae508941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50896, %struct.ScmObj* %argslist54158$ae508940)
store volatile %struct.ScmObj* %argslist54158$ae508941, %struct.ScmObj** %stackaddr$prim54983, align 8
%stackaddr$prim54984 = alloca %struct.ScmObj*, align 8
%argslist54158$ae508942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50895, %struct.ScmObj* %argslist54158$ae508941)
store volatile %struct.ScmObj* %argslist54158$ae508942, %struct.ScmObj** %stackaddr$prim54984, align 8
%clofunc54985 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50894)
musttail call tailcc void %clofunc54985(%struct.ScmObj* %ae50894, %struct.ScmObj* %argslist54158$ae508942)
ret void
}

define tailcc void @proc_clo$ae50894(%struct.ScmObj* %env$ae50894,%struct.ScmObj* %current_45args54050) {
%stackaddr$env-ref54986 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50894, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54986
%stackaddr$env-ref54987 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50894, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54987
%stackaddr$env-ref54988 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50894, i64 2)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref54988
%stackaddr$env-ref54989 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50894, i64 3)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref54989
%stackaddr$prim54990 = alloca %struct.ScmObj*, align 8
%_95k47373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54050)
store volatile %struct.ScmObj* %_95k47373, %struct.ScmObj** %stackaddr$prim54990, align 8
%stackaddr$prim54991 = alloca %struct.ScmObj*, align 8
%current_45args54051 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54050)
store volatile %struct.ScmObj* %current_45args54051, %struct.ScmObj** %stackaddr$prim54991, align 8
%stackaddr$prim54992 = alloca %struct.ScmObj*, align 8
%_37fourth47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54051)
store volatile %struct.ScmObj* %_37fourth47126, %struct.ScmObj** %stackaddr$prim54992, align 8
%stackaddr$makeclosure54993 = alloca %struct.ScmObj*, align 8
%fptrToInt54994 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50920 to i64
%ae50920 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54994)
store volatile %struct.ScmObj* %ae50920, %struct.ScmObj** %stackaddr$makeclosure54993, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50920, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50920, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50920, %struct.ScmObj* %_37first47132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50920, %struct.ScmObj* %_37second47130, i64 3)
%ae50921 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54995 = alloca %struct.ScmObj*, align 8
%fptrToInt54996 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50922 to i64
%ae50922 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54996)
store volatile %struct.ScmObj* %ae50922, %struct.ScmObj** %stackaddr$makeclosure54995, align 8
%argslist54153$ae509200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54997 = alloca %struct.ScmObj*, align 8
%argslist54153$ae509201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50922, %struct.ScmObj* %argslist54153$ae509200)
store volatile %struct.ScmObj* %argslist54153$ae509201, %struct.ScmObj** %stackaddr$prim54997, align 8
%stackaddr$prim54998 = alloca %struct.ScmObj*, align 8
%argslist54153$ae509202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50921, %struct.ScmObj* %argslist54153$ae509201)
store volatile %struct.ScmObj* %argslist54153$ae509202, %struct.ScmObj** %stackaddr$prim54998, align 8
%clofunc54999 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50920)
musttail call tailcc void %clofunc54999(%struct.ScmObj* %ae50920, %struct.ScmObj* %argslist54153$ae509202)
ret void
}

define tailcc void @proc_clo$ae50920(%struct.ScmObj* %env$ae50920,%struct.ScmObj* %current_45args54053) {
%stackaddr$env-ref55000 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50920, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55000
%stackaddr$env-ref55001 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50920, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55001
%stackaddr$env-ref55002 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50920, i64 2)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref55002
%stackaddr$env-ref55003 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50920, i64 3)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55003
%stackaddr$prim55004 = alloca %struct.ScmObj*, align 8
%_95k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54053)
store volatile %struct.ScmObj* %_95k47374, %struct.ScmObj** %stackaddr$prim55004, align 8
%stackaddr$prim55005 = alloca %struct.ScmObj*, align 8
%current_45args54054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54053)
store volatile %struct.ScmObj* %current_45args54054, %struct.ScmObj** %stackaddr$prim55005, align 8
%stackaddr$prim55006 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54054)
store volatile %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$prim55006, align 8
%stackaddr$makeclosure55007 = alloca %struct.ScmObj*, align 8
%fptrToInt55008 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50958 to i64
%ae50958 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55008)
store volatile %struct.ScmObj* %ae50958, %struct.ScmObj** %stackaddr$makeclosure55007, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50958, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50958, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50958, %struct.ScmObj* %_37first47132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50958, %struct.ScmObj* %_37second47130, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae50958, %struct.ScmObj* %anf_45bind47321, i64 4)
%ae50959 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55009 = alloca %struct.ScmObj*, align 8
%fptrToInt55010 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50960 to i64
%ae50960 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55010)
store volatile %struct.ScmObj* %ae50960, %struct.ScmObj** %stackaddr$makeclosure55009, align 8
%argslist54143$ae509580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55011 = alloca %struct.ScmObj*, align 8
%argslist54143$ae509581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50960, %struct.ScmObj* %argslist54143$ae509580)
store volatile %struct.ScmObj* %argslist54143$ae509581, %struct.ScmObj** %stackaddr$prim55011, align 8
%stackaddr$prim55012 = alloca %struct.ScmObj*, align 8
%argslist54143$ae509582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50959, %struct.ScmObj* %argslist54143$ae509581)
store volatile %struct.ScmObj* %argslist54143$ae509582, %struct.ScmObj** %stackaddr$prim55012, align 8
%clofunc55013 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50958)
musttail call tailcc void %clofunc55013(%struct.ScmObj* %ae50958, %struct.ScmObj* %argslist54143$ae509582)
ret void
}

define tailcc void @proc_clo$ae50958(%struct.ScmObj* %env$ae50958,%struct.ScmObj* %current_45args54056) {
%stackaddr$env-ref55014 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50958, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55014
%stackaddr$env-ref55015 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50958, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55015
%stackaddr$env-ref55016 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50958, i64 2)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref55016
%stackaddr$env-ref55017 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50958, i64 3)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55017
%stackaddr$env-ref55018 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50958, i64 4)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55018
%stackaddr$prim55019 = alloca %struct.ScmObj*, align 8
%_95k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54056)
store volatile %struct.ScmObj* %_95k47375, %struct.ScmObj** %stackaddr$prim55019, align 8
%stackaddr$prim55020 = alloca %struct.ScmObj*, align 8
%current_45args54057 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54056)
store volatile %struct.ScmObj* %current_45args54057, %struct.ScmObj** %stackaddr$prim55020, align 8
%stackaddr$prim55021 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54057)
store volatile %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$prim55021, align 8
%stackaddr$makeclosure55022 = alloca %struct.ScmObj*, align 8
%fptrToInt55023 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50980 to i64
%ae50980 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55023)
store volatile %struct.ScmObj* %ae50980, %struct.ScmObj** %stackaddr$makeclosure55022, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50980, %struct.ScmObj* %_37second47130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50980, %struct.ScmObj* %anf_45bind47322, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50980, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50980, %struct.ScmObj* %_37foldr147090, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae50980, %struct.ScmObj* %_37first47132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae50980, %struct.ScmObj* %anf_45bind47321, i64 5)
%ae50981 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55024 = alloca %struct.ScmObj*, align 8
%fptrToInt55025 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50982 to i64
%ae50982 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55025)
store volatile %struct.ScmObj* %ae50982, %struct.ScmObj** %stackaddr$makeclosure55024, align 8
%argslist54141$ae509800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55026 = alloca %struct.ScmObj*, align 8
%argslist54141$ae509801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50982, %struct.ScmObj* %argslist54141$ae509800)
store volatile %struct.ScmObj* %argslist54141$ae509801, %struct.ScmObj** %stackaddr$prim55026, align 8
%stackaddr$prim55027 = alloca %struct.ScmObj*, align 8
%argslist54141$ae509802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50981, %struct.ScmObj* %argslist54141$ae509801)
store volatile %struct.ScmObj* %argslist54141$ae509802, %struct.ScmObj** %stackaddr$prim55027, align 8
%clofunc55028 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50980)
musttail call tailcc void %clofunc55028(%struct.ScmObj* %ae50980, %struct.ScmObj* %argslist54141$ae509802)
ret void
}

define tailcc void @proc_clo$ae50980(%struct.ScmObj* %env$ae50980,%struct.ScmObj* %current_45args54059) {
%stackaddr$env-ref55029 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50980, i64 0)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55029
%stackaddr$env-ref55030 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50980, i64 1)
store %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$env-ref55030
%stackaddr$env-ref55031 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50980, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55031
%stackaddr$env-ref55032 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50980, i64 3)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55032
%stackaddr$env-ref55033 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50980, i64 4)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref55033
%stackaddr$env-ref55034 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50980, i64 5)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55034
%stackaddr$prim55035 = alloca %struct.ScmObj*, align 8
%_95k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54059)
store volatile %struct.ScmObj* %_95k47376, %struct.ScmObj** %stackaddr$prim55035, align 8
%stackaddr$prim55036 = alloca %struct.ScmObj*, align 8
%current_45args54060 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54059)
store volatile %struct.ScmObj* %current_45args54060, %struct.ScmObj** %stackaddr$prim55036, align 8
%stackaddr$prim55037 = alloca %struct.ScmObj*, align 8
%anf_45bind47323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54060)
store volatile %struct.ScmObj* %anf_45bind47323, %struct.ScmObj** %stackaddr$prim55037, align 8
%stackaddr$makeclosure55038 = alloca %struct.ScmObj*, align 8
%fptrToInt55039 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51003 to i64
%ae51003 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55039)
store volatile %struct.ScmObj* %ae51003, %struct.ScmObj** %stackaddr$makeclosure55038, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %_37second47130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %anf_45bind47322, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %_37foldr147090, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %_37first47132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %anf_45bind47321, i64 5)
%ae51004 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51005 = call %struct.ScmObj* @const_init_int(i64 1)
%ae51006 = call %struct.ScmObj* @const_init_int(i64 2)
%ae51007 = call %struct.ScmObj* @const_init_int(i64 3)
%argslist54139$anf_45bind473230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55040 = alloca %struct.ScmObj*, align 8
%argslist54139$anf_45bind473231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51007, %struct.ScmObj* %argslist54139$anf_45bind473230)
store volatile %struct.ScmObj* %argslist54139$anf_45bind473231, %struct.ScmObj** %stackaddr$prim55040, align 8
%stackaddr$prim55041 = alloca %struct.ScmObj*, align 8
%argslist54139$anf_45bind473232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51006, %struct.ScmObj* %argslist54139$anf_45bind473231)
store volatile %struct.ScmObj* %argslist54139$anf_45bind473232, %struct.ScmObj** %stackaddr$prim55041, align 8
%stackaddr$prim55042 = alloca %struct.ScmObj*, align 8
%argslist54139$anf_45bind473233 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51005, %struct.ScmObj* %argslist54139$anf_45bind473232)
store volatile %struct.ScmObj* %argslist54139$anf_45bind473233, %struct.ScmObj** %stackaddr$prim55042, align 8
%stackaddr$prim55043 = alloca %struct.ScmObj*, align 8
%argslist54139$anf_45bind473234 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51004, %struct.ScmObj* %argslist54139$anf_45bind473233)
store volatile %struct.ScmObj* %argslist54139$anf_45bind473234, %struct.ScmObj** %stackaddr$prim55043, align 8
%stackaddr$prim55044 = alloca %struct.ScmObj*, align 8
%argslist54139$anf_45bind473235 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51003, %struct.ScmObj* %argslist54139$anf_45bind473234)
store volatile %struct.ScmObj* %argslist54139$anf_45bind473235, %struct.ScmObj** %stackaddr$prim55044, align 8
%clofunc55045 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47323)
musttail call tailcc void %clofunc55045(%struct.ScmObj* %anf_45bind47323, %struct.ScmObj* %argslist54139$anf_45bind473235)
ret void
}

define tailcc void @proc_clo$ae51003(%struct.ScmObj* %env$ae51003,%struct.ScmObj* %current_45args54062) {
%stackaddr$env-ref55046 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 0)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55046
%stackaddr$env-ref55047 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 1)
store %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$env-ref55047
%stackaddr$env-ref55048 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55048
%stackaddr$env-ref55049 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 3)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55049
%stackaddr$env-ref55050 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 4)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref55050
%stackaddr$env-ref55051 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 5)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55051
%stackaddr$prim55052 = alloca %struct.ScmObj*, align 8
%_95k47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54062)
store volatile %struct.ScmObj* %_95k47377, %struct.ScmObj** %stackaddr$prim55052, align 8
%stackaddr$prim55053 = alloca %struct.ScmObj*, align 8
%current_45args54063 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54062)
store volatile %struct.ScmObj* %current_45args54063, %struct.ScmObj** %stackaddr$prim55053, align 8
%stackaddr$prim55054 = alloca %struct.ScmObj*, align 8
%anf_45bind47324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54063)
store volatile %struct.ScmObj* %anf_45bind47324, %struct.ScmObj** %stackaddr$prim55054, align 8
%stackaddr$makeclosure55055 = alloca %struct.ScmObj*, align 8
%fptrToInt55056 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51032 to i64
%ae51032 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55056)
store volatile %struct.ScmObj* %ae51032, %struct.ScmObj** %stackaddr$makeclosure55055, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %_37first47132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %anf_45bind47324, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %_37second47130, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %anf_45bind47322, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %_37foldl47172, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %_37foldr147090, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %anf_45bind47321, i64 6)
%ae51033 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55057 = alloca %struct.ScmObj*, align 8
%fptrToInt55058 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51034 to i64
%ae51034 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55058)
store volatile %struct.ScmObj* %ae51034, %struct.ScmObj** %stackaddr$makeclosure55057, align 8
%argslist54138$ae510320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55059 = alloca %struct.ScmObj*, align 8
%argslist54138$ae510321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51034, %struct.ScmObj* %argslist54138$ae510320)
store volatile %struct.ScmObj* %argslist54138$ae510321, %struct.ScmObj** %stackaddr$prim55059, align 8
%stackaddr$prim55060 = alloca %struct.ScmObj*, align 8
%argslist54138$ae510322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51033, %struct.ScmObj* %argslist54138$ae510321)
store volatile %struct.ScmObj* %argslist54138$ae510322, %struct.ScmObj** %stackaddr$prim55060, align 8
%clofunc55061 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51032)
musttail call tailcc void %clofunc55061(%struct.ScmObj* %ae51032, %struct.ScmObj* %argslist54138$ae510322)
ret void
}

define tailcc void @proc_clo$ae51032(%struct.ScmObj* %env$ae51032,%struct.ScmObj* %current_45args54065) {
%stackaddr$env-ref55062 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 0)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref55062
%stackaddr$env-ref55063 = alloca %struct.ScmObj*, align 8
%anf_45bind47324 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 1)
store %struct.ScmObj* %anf_45bind47324, %struct.ScmObj** %stackaddr$env-ref55063
%stackaddr$env-ref55064 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 2)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55064
%stackaddr$env-ref55065 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 3)
store %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$env-ref55065
%stackaddr$env-ref55066 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 4)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55066
%stackaddr$env-ref55067 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 5)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55067
%stackaddr$env-ref55068 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 6)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55068
%stackaddr$prim55069 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54065)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim55069, align 8
%stackaddr$prim55070 = alloca %struct.ScmObj*, align 8
%current_45args54066 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54065)
store volatile %struct.ScmObj* %current_45args54066, %struct.ScmObj** %stackaddr$prim55070, align 8
%stackaddr$prim55071 = alloca %struct.ScmObj*, align 8
%anf_45bind47325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54066)
store volatile %struct.ScmObj* %anf_45bind47325, %struct.ScmObj** %stackaddr$prim55071, align 8
%stackaddr$makeclosure55072 = alloca %struct.ScmObj*, align 8
%fptrToInt55073 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51055 to i64
%ae51055 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55073)
store volatile %struct.ScmObj* %ae51055, %struct.ScmObj** %stackaddr$makeclosure55072, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %_37first47132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %anf_45bind47324, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %_37second47130, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %anf_45bind47322, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %_37foldl47172, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %_37foldr147090, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %anf_45bind47321, i64 6)
%ae51056 = call %struct.ScmObj* @const_init_int(i64 4)
%ae51057 = call %struct.ScmObj* @const_init_int(i64 5)
%ae51058 = call %struct.ScmObj* @const_init_int(i64 6)
%ae51059 = call %struct.ScmObj* @const_init_int(i64 7)
%argslist54136$anf_45bind473250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55074 = alloca %struct.ScmObj*, align 8
%argslist54136$anf_45bind473251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51059, %struct.ScmObj* %argslist54136$anf_45bind473250)
store volatile %struct.ScmObj* %argslist54136$anf_45bind473251, %struct.ScmObj** %stackaddr$prim55074, align 8
%stackaddr$prim55075 = alloca %struct.ScmObj*, align 8
%argslist54136$anf_45bind473252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51058, %struct.ScmObj* %argslist54136$anf_45bind473251)
store volatile %struct.ScmObj* %argslist54136$anf_45bind473252, %struct.ScmObj** %stackaddr$prim55075, align 8
%stackaddr$prim55076 = alloca %struct.ScmObj*, align 8
%argslist54136$anf_45bind473253 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51057, %struct.ScmObj* %argslist54136$anf_45bind473252)
store volatile %struct.ScmObj* %argslist54136$anf_45bind473253, %struct.ScmObj** %stackaddr$prim55076, align 8
%stackaddr$prim55077 = alloca %struct.ScmObj*, align 8
%argslist54136$anf_45bind473254 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51056, %struct.ScmObj* %argslist54136$anf_45bind473253)
store volatile %struct.ScmObj* %argslist54136$anf_45bind473254, %struct.ScmObj** %stackaddr$prim55077, align 8
%stackaddr$prim55078 = alloca %struct.ScmObj*, align 8
%argslist54136$anf_45bind473255 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51055, %struct.ScmObj* %argslist54136$anf_45bind473254)
store volatile %struct.ScmObj* %argslist54136$anf_45bind473255, %struct.ScmObj** %stackaddr$prim55078, align 8
%clofunc55079 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47325)
musttail call tailcc void %clofunc55079(%struct.ScmObj* %anf_45bind47325, %struct.ScmObj* %argslist54136$anf_45bind473255)
ret void
}

define tailcc void @proc_clo$ae51055(%struct.ScmObj* %env$ae51055,%struct.ScmObj* %current_45args54068) {
%stackaddr$env-ref55080 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 0)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref55080
%stackaddr$env-ref55081 = alloca %struct.ScmObj*, align 8
%anf_45bind47324 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 1)
store %struct.ScmObj* %anf_45bind47324, %struct.ScmObj** %stackaddr$env-ref55081
%stackaddr$env-ref55082 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 2)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55082
%stackaddr$env-ref55083 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 3)
store %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$env-ref55083
%stackaddr$env-ref55084 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 4)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55084
%stackaddr$env-ref55085 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 5)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55085
%stackaddr$env-ref55086 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 6)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55086
%stackaddr$prim55087 = alloca %struct.ScmObj*, align 8
%_95k47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54068)
store volatile %struct.ScmObj* %_95k47379, %struct.ScmObj** %stackaddr$prim55087, align 8
%stackaddr$prim55088 = alloca %struct.ScmObj*, align 8
%current_45args54069 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54068)
store volatile %struct.ScmObj* %current_45args54069, %struct.ScmObj** %stackaddr$prim55088, align 8
%stackaddr$prim55089 = alloca %struct.ScmObj*, align 8
%anf_45bind47326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54069)
store volatile %struct.ScmObj* %anf_45bind47326, %struct.ScmObj** %stackaddr$prim55089, align 8
%stackaddr$makeclosure55090 = alloca %struct.ScmObj*, align 8
%fptrToInt55091 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51084 to i64
%ae51084 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55091)
store volatile %struct.ScmObj* %ae51084, %struct.ScmObj** %stackaddr$makeclosure55090, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51084, %struct.ScmObj* %_37first47132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51084, %struct.ScmObj* %anf_45bind47324, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51084, %struct.ScmObj* %_37second47130, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51084, %struct.ScmObj* %anf_45bind47322, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51084, %struct.ScmObj* %_37foldl47172, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51084, %struct.ScmObj* %_37foldr147090, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51084, %struct.ScmObj* %anf_45bind47326, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51084, %struct.ScmObj* %anf_45bind47321, i64 7)
%ae51085 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55092 = alloca %struct.ScmObj*, align 8
%fptrToInt55093 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51086 to i64
%ae51086 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55093)
store volatile %struct.ScmObj* %ae51086, %struct.ScmObj** %stackaddr$makeclosure55092, align 8
%argslist54135$ae510840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55094 = alloca %struct.ScmObj*, align 8
%argslist54135$ae510841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51086, %struct.ScmObj* %argslist54135$ae510840)
store volatile %struct.ScmObj* %argslist54135$ae510841, %struct.ScmObj** %stackaddr$prim55094, align 8
%stackaddr$prim55095 = alloca %struct.ScmObj*, align 8
%argslist54135$ae510842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51085, %struct.ScmObj* %argslist54135$ae510841)
store volatile %struct.ScmObj* %argslist54135$ae510842, %struct.ScmObj** %stackaddr$prim55095, align 8
%clofunc55096 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51084)
musttail call tailcc void %clofunc55096(%struct.ScmObj* %ae51084, %struct.ScmObj* %argslist54135$ae510842)
ret void
}

define tailcc void @proc_clo$ae51084(%struct.ScmObj* %env$ae51084,%struct.ScmObj* %current_45args54071) {
%stackaddr$env-ref55097 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51084, i64 0)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref55097
%stackaddr$env-ref55098 = alloca %struct.ScmObj*, align 8
%anf_45bind47324 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51084, i64 1)
store %struct.ScmObj* %anf_45bind47324, %struct.ScmObj** %stackaddr$env-ref55098
%stackaddr$env-ref55099 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51084, i64 2)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55099
%stackaddr$env-ref55100 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51084, i64 3)
store %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$env-ref55100
%stackaddr$env-ref55101 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51084, i64 4)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55101
%stackaddr$env-ref55102 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51084, i64 5)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55102
%stackaddr$env-ref55103 = alloca %struct.ScmObj*, align 8
%anf_45bind47326 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51084, i64 6)
store %struct.ScmObj* %anf_45bind47326, %struct.ScmObj** %stackaddr$env-ref55103
%stackaddr$env-ref55104 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51084, i64 7)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55104
%stackaddr$prim55105 = alloca %struct.ScmObj*, align 8
%_95k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54071)
store volatile %struct.ScmObj* %_95k47380, %struct.ScmObj** %stackaddr$prim55105, align 8
%stackaddr$prim55106 = alloca %struct.ScmObj*, align 8
%current_45args54072 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54071)
store volatile %struct.ScmObj* %current_45args54072, %struct.ScmObj** %stackaddr$prim55106, align 8
%stackaddr$prim55107 = alloca %struct.ScmObj*, align 8
%anf_45bind47327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54072)
store volatile %struct.ScmObj* %anf_45bind47327, %struct.ScmObj** %stackaddr$prim55107, align 8
%stackaddr$makeclosure55108 = alloca %struct.ScmObj*, align 8
%fptrToInt55109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51107 to i64
%ae51107 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55109)
store volatile %struct.ScmObj* %ae51107, %struct.ScmObj** %stackaddr$makeclosure55108, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %_37first47132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %anf_45bind47324, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %_37second47130, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %anf_45bind47322, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %_37foldl47172, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %_37foldr147090, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %anf_45bind47326, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %anf_45bind47321, i64 7)
%ae51108 = call %struct.ScmObj* @const_init_int(i64 2)
%ae51109 = call %struct.ScmObj* @const_init_int(i64 5)
%ae51110 = call %struct.ScmObj* @const_init_int(i64 7)
%ae51111 = call %struct.ScmObj* @const_init_int(i64 9)
%argslist54133$anf_45bind473270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55110 = alloca %struct.ScmObj*, align 8
%argslist54133$anf_45bind473271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51111, %struct.ScmObj* %argslist54133$anf_45bind473270)
store volatile %struct.ScmObj* %argslist54133$anf_45bind473271, %struct.ScmObj** %stackaddr$prim55110, align 8
%stackaddr$prim55111 = alloca %struct.ScmObj*, align 8
%argslist54133$anf_45bind473272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51110, %struct.ScmObj* %argslist54133$anf_45bind473271)
store volatile %struct.ScmObj* %argslist54133$anf_45bind473272, %struct.ScmObj** %stackaddr$prim55111, align 8
%stackaddr$prim55112 = alloca %struct.ScmObj*, align 8
%argslist54133$anf_45bind473273 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51109, %struct.ScmObj* %argslist54133$anf_45bind473272)
store volatile %struct.ScmObj* %argslist54133$anf_45bind473273, %struct.ScmObj** %stackaddr$prim55112, align 8
%stackaddr$prim55113 = alloca %struct.ScmObj*, align 8
%argslist54133$anf_45bind473274 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51108, %struct.ScmObj* %argslist54133$anf_45bind473273)
store volatile %struct.ScmObj* %argslist54133$anf_45bind473274, %struct.ScmObj** %stackaddr$prim55113, align 8
%stackaddr$prim55114 = alloca %struct.ScmObj*, align 8
%argslist54133$anf_45bind473275 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51107, %struct.ScmObj* %argslist54133$anf_45bind473274)
store volatile %struct.ScmObj* %argslist54133$anf_45bind473275, %struct.ScmObj** %stackaddr$prim55114, align 8
%clofunc55115 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47327)
musttail call tailcc void %clofunc55115(%struct.ScmObj* %anf_45bind47327, %struct.ScmObj* %argslist54133$anf_45bind473275)
ret void
}

define tailcc void @proc_clo$ae51107(%struct.ScmObj* %env$ae51107,%struct.ScmObj* %current_45args54074) {
%stackaddr$env-ref55116 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 0)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref55116
%stackaddr$env-ref55117 = alloca %struct.ScmObj*, align 8
%anf_45bind47324 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 1)
store %struct.ScmObj* %anf_45bind47324, %struct.ScmObj** %stackaddr$env-ref55117
%stackaddr$env-ref55118 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 2)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55118
%stackaddr$env-ref55119 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 3)
store %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$env-ref55119
%stackaddr$env-ref55120 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 4)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55120
%stackaddr$env-ref55121 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 5)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55121
%stackaddr$env-ref55122 = alloca %struct.ScmObj*, align 8
%anf_45bind47326 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 6)
store %struct.ScmObj* %anf_45bind47326, %struct.ScmObj** %stackaddr$env-ref55122
%stackaddr$env-ref55123 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 7)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55123
%stackaddr$prim55124 = alloca %struct.ScmObj*, align 8
%_95k47381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54074)
store volatile %struct.ScmObj* %_95k47381, %struct.ScmObj** %stackaddr$prim55124, align 8
%stackaddr$prim55125 = alloca %struct.ScmObj*, align 8
%current_45args54075 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54074)
store volatile %struct.ScmObj* %current_45args54075, %struct.ScmObj** %stackaddr$prim55125, align 8
%stackaddr$prim55126 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54075)
store volatile %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$prim55126, align 8
%stackaddr$makeclosure55127 = alloca %struct.ScmObj*, align 8
%fptrToInt55128 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51137 to i64
%ae51137 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55128)
store volatile %struct.ScmObj* %ae51137, %struct.ScmObj** %stackaddr$makeclosure55127, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51137, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51137, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51137, %struct.ScmObj* %_37first47132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51137, %struct.ScmObj* %_37second47130, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51137, %struct.ScmObj* %anf_45bind47321, i64 4)
%argslist54132$anf_45bind473220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55129 = alloca %struct.ScmObj*, align 8
%argslist54132$anf_45bind473221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47328, %struct.ScmObj* %argslist54132$anf_45bind473220)
store volatile %struct.ScmObj* %argslist54132$anf_45bind473221, %struct.ScmObj** %stackaddr$prim55129, align 8
%stackaddr$prim55130 = alloca %struct.ScmObj*, align 8
%argslist54132$anf_45bind473222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47326, %struct.ScmObj* %argslist54132$anf_45bind473221)
store volatile %struct.ScmObj* %argslist54132$anf_45bind473222, %struct.ScmObj** %stackaddr$prim55130, align 8
%stackaddr$prim55131 = alloca %struct.ScmObj*, align 8
%argslist54132$anf_45bind473223 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47324, %struct.ScmObj* %argslist54132$anf_45bind473222)
store volatile %struct.ScmObj* %argslist54132$anf_45bind473223, %struct.ScmObj** %stackaddr$prim55131, align 8
%stackaddr$prim55132 = alloca %struct.ScmObj*, align 8
%argslist54132$anf_45bind473224 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51137, %struct.ScmObj* %argslist54132$anf_45bind473223)
store volatile %struct.ScmObj* %argslist54132$anf_45bind473224, %struct.ScmObj** %stackaddr$prim55132, align 8
%clofunc55133 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47322)
musttail call tailcc void %clofunc55133(%struct.ScmObj* %anf_45bind47322, %struct.ScmObj* %argslist54132$anf_45bind473224)
ret void
}

define tailcc void @proc_clo$ae51137(%struct.ScmObj* %env$ae51137,%struct.ScmObj* %current_45args54077) {
%stackaddr$env-ref55134 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51137, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55134
%stackaddr$env-ref55135 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51137, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55135
%stackaddr$env-ref55136 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51137, i64 2)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref55136
%stackaddr$env-ref55137 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51137, i64 3)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55137
%stackaddr$env-ref55138 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51137, i64 4)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55138
%stackaddr$prim55139 = alloca %struct.ScmObj*, align 8
%_95k47382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54077)
store volatile %struct.ScmObj* %_95k47382, %struct.ScmObj** %stackaddr$prim55139, align 8
%stackaddr$prim55140 = alloca %struct.ScmObj*, align 8
%current_45args54078 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54077)
store volatile %struct.ScmObj* %current_45args54078, %struct.ScmObj** %stackaddr$prim55140, align 8
%stackaddr$prim55141 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54078)
store volatile %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$prim55141, align 8
%stackaddr$makeclosure55142 = alloca %struct.ScmObj*, align 8
%fptrToInt55143 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51141 to i64
%ae51141 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55143)
store volatile %struct.ScmObj* %ae51141, %struct.ScmObj** %stackaddr$makeclosure55142, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51141, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51141, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51141, %struct.ScmObj* %anf_45bind47329, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51141, %struct.ScmObj* %_37first47132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51141, %struct.ScmObj* %_37second47130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51141, %struct.ScmObj* %anf_45bind47321, i64 5)
%ae51142 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55144 = alloca %struct.ScmObj*, align 8
%fptrToInt55145 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51143 to i64
%ae51143 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55145)
store volatile %struct.ScmObj* %ae51143, %struct.ScmObj** %stackaddr$makeclosure55144, align 8
%argslist54131$ae511410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55146 = alloca %struct.ScmObj*, align 8
%argslist54131$ae511411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51143, %struct.ScmObj* %argslist54131$ae511410)
store volatile %struct.ScmObj* %argslist54131$ae511411, %struct.ScmObj** %stackaddr$prim55146, align 8
%stackaddr$prim55147 = alloca %struct.ScmObj*, align 8
%argslist54131$ae511412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51142, %struct.ScmObj* %argslist54131$ae511411)
store volatile %struct.ScmObj* %argslist54131$ae511412, %struct.ScmObj** %stackaddr$prim55147, align 8
%clofunc55148 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51141)
musttail call tailcc void %clofunc55148(%struct.ScmObj* %ae51141, %struct.ScmObj* %argslist54131$ae511412)
ret void
}

define tailcc void @proc_clo$ae51141(%struct.ScmObj* %env$ae51141,%struct.ScmObj* %current_45args54080) {
%stackaddr$env-ref55149 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51141, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55149
%stackaddr$env-ref55150 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51141, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55150
%stackaddr$env-ref55151 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51141, i64 2)
store %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$env-ref55151
%stackaddr$env-ref55152 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51141, i64 3)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref55152
%stackaddr$env-ref55153 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51141, i64 4)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55153
%stackaddr$env-ref55154 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51141, i64 5)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55154
%stackaddr$prim55155 = alloca %struct.ScmObj*, align 8
%_95k47383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54080)
store volatile %struct.ScmObj* %_95k47383, %struct.ScmObj** %stackaddr$prim55155, align 8
%stackaddr$prim55156 = alloca %struct.ScmObj*, align 8
%current_45args54081 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54080)
store volatile %struct.ScmObj* %current_45args54081, %struct.ScmObj** %stackaddr$prim55156, align 8
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54081)
store volatile %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$prim55157, align 8
%stackaddr$makeclosure55158 = alloca %struct.ScmObj*, align 8
%fptrToInt55159 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51163 to i64
%ae51163 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55159)
store volatile %struct.ScmObj* %ae51163, %struct.ScmObj** %stackaddr$makeclosure55158, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51163, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51163, %struct.ScmObj* %anf_45bind47330, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51163, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51163, %struct.ScmObj* %anf_45bind47329, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51163, %struct.ScmObj* %anf_45bind47321, i64 4)
%ae51164 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55160 = alloca %struct.ScmObj*, align 8
%fptrToInt55161 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51165 to i64
%ae51165 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55161)
store volatile %struct.ScmObj* %ae51165, %struct.ScmObj** %stackaddr$makeclosure55160, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51165, %struct.ScmObj* %_37first47132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51165, %struct.ScmObj* %_37second47130, i64 1)
%argslist54129$ae511630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55162 = alloca %struct.ScmObj*, align 8
%argslist54129$ae511631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51165, %struct.ScmObj* %argslist54129$ae511630)
store volatile %struct.ScmObj* %argslist54129$ae511631, %struct.ScmObj** %stackaddr$prim55162, align 8
%stackaddr$prim55163 = alloca %struct.ScmObj*, align 8
%argslist54129$ae511632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51164, %struct.ScmObj* %argslist54129$ae511631)
store volatile %struct.ScmObj* %argslist54129$ae511632, %struct.ScmObj** %stackaddr$prim55163, align 8
%clofunc55164 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51163)
musttail call tailcc void %clofunc55164(%struct.ScmObj* %ae51163, %struct.ScmObj* %argslist54129$ae511632)
ret void
}

define tailcc void @proc_clo$ae51163(%struct.ScmObj* %env$ae51163,%struct.ScmObj* %current_45args54083) {
%stackaddr$env-ref55165 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51163, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55165
%stackaddr$env-ref55166 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51163, i64 1)
store %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$env-ref55166
%stackaddr$env-ref55167 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51163, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55167
%stackaddr$env-ref55168 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51163, i64 3)
store %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$env-ref55168
%stackaddr$env-ref55169 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51163, i64 4)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55169
%stackaddr$prim55170 = alloca %struct.ScmObj*, align 8
%_95k47384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54083)
store volatile %struct.ScmObj* %_95k47384, %struct.ScmObj** %stackaddr$prim55170, align 8
%stackaddr$prim55171 = alloca %struct.ScmObj*, align 8
%current_45args54084 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54083)
store volatile %struct.ScmObj* %current_45args54084, %struct.ScmObj** %stackaddr$prim55171, align 8
%stackaddr$prim55172 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54084)
store volatile %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$prim55172, align 8
%stackaddr$makeclosure55173 = alloca %struct.ScmObj*, align 8
%fptrToInt55174 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51235 to i64
%ae51235 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55174)
store volatile %struct.ScmObj* %ae51235, %struct.ScmObj** %stackaddr$makeclosure55173, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51235, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51235, %struct.ScmObj* %anf_45bind47330, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51235, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51235, %struct.ScmObj* %anf_45bind47329, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51235, %struct.ScmObj* %anf_45bind47321, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51235, %struct.ScmObj* %anf_45bind47335, i64 5)
%ae51236 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55175 = alloca %struct.ScmObj*, align 8
%fptrToInt55176 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51237 to i64
%ae51237 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55176)
store volatile %struct.ScmObj* %ae51237, %struct.ScmObj** %stackaddr$makeclosure55175, align 8
%argslist54115$ae512350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55177 = alloca %struct.ScmObj*, align 8
%argslist54115$ae512351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51237, %struct.ScmObj* %argslist54115$ae512350)
store volatile %struct.ScmObj* %argslist54115$ae512351, %struct.ScmObj** %stackaddr$prim55177, align 8
%stackaddr$prim55178 = alloca %struct.ScmObj*, align 8
%argslist54115$ae512352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51236, %struct.ScmObj* %argslist54115$ae512351)
store volatile %struct.ScmObj* %argslist54115$ae512352, %struct.ScmObj** %stackaddr$prim55178, align 8
%clofunc55179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51235)
musttail call tailcc void %clofunc55179(%struct.ScmObj* %ae51235, %struct.ScmObj* %argslist54115$ae512352)
ret void
}

define tailcc void @proc_clo$ae51235(%struct.ScmObj* %env$ae51235,%struct.ScmObj* %current_45args54086) {
%stackaddr$env-ref55180 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51235, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55180
%stackaddr$env-ref55181 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51235, i64 1)
store %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$env-ref55181
%stackaddr$env-ref55182 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51235, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55182
%stackaddr$env-ref55183 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51235, i64 3)
store %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$env-ref55183
%stackaddr$env-ref55184 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51235, i64 4)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55184
%stackaddr$env-ref55185 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51235, i64 5)
store %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$env-ref55185
%stackaddr$prim55186 = alloca %struct.ScmObj*, align 8
%_95k47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54086)
store volatile %struct.ScmObj* %_95k47385, %struct.ScmObj** %stackaddr$prim55186, align 8
%stackaddr$prim55187 = alloca %struct.ScmObj*, align 8
%current_45args54087 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54086)
store volatile %struct.ScmObj* %current_45args54087, %struct.ScmObj** %stackaddr$prim55187, align 8
%stackaddr$prim55188 = alloca %struct.ScmObj*, align 8
%anf_45bind47338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54087)
store volatile %struct.ScmObj* %anf_45bind47338, %struct.ScmObj** %stackaddr$prim55188, align 8
%stackaddr$makeclosure55189 = alloca %struct.ScmObj*, align 8
%fptrToInt55190 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51265 to i64
%ae51265 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55190)
store volatile %struct.ScmObj* %ae51265, %struct.ScmObj** %stackaddr$makeclosure55189, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51265, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51265, %struct.ScmObj* %anf_45bind47330, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51265, %struct.ScmObj* %anf_45bind47329, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51265, %struct.ScmObj* %anf_45bind47338, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51265, %struct.ScmObj* %anf_45bind47321, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51265, %struct.ScmObj* %anf_45bind47335, i64 5)
%ae51266 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55191 = alloca %struct.ScmObj*, align 8
%fptrToInt55192 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51267 to i64
%ae51267 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55192)
store volatile %struct.ScmObj* %ae51267, %struct.ScmObj** %stackaddr$makeclosure55191, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51267, %struct.ScmObj* %_37foldr147090, i64 0)
%argslist54107$ae512650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55193 = alloca %struct.ScmObj*, align 8
%argslist54107$ae512651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51267, %struct.ScmObj* %argslist54107$ae512650)
store volatile %struct.ScmObj* %argslist54107$ae512651, %struct.ScmObj** %stackaddr$prim55193, align 8
%stackaddr$prim55194 = alloca %struct.ScmObj*, align 8
%argslist54107$ae512652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51266, %struct.ScmObj* %argslist54107$ae512651)
store volatile %struct.ScmObj* %argslist54107$ae512652, %struct.ScmObj** %stackaddr$prim55194, align 8
%clofunc55195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51265)
musttail call tailcc void %clofunc55195(%struct.ScmObj* %ae51265, %struct.ScmObj* %argslist54107$ae512652)
ret void
}

define tailcc void @proc_clo$ae51265(%struct.ScmObj* %env$ae51265,%struct.ScmObj* %current_45args54089) {
%stackaddr$env-ref55196 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51265, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55196
%stackaddr$env-ref55197 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51265, i64 1)
store %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$env-ref55197
%stackaddr$env-ref55198 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51265, i64 2)
store %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$env-ref55198
%stackaddr$env-ref55199 = alloca %struct.ScmObj*, align 8
%anf_45bind47338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51265, i64 3)
store %struct.ScmObj* %anf_45bind47338, %struct.ScmObj** %stackaddr$env-ref55199
%stackaddr$env-ref55200 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51265, i64 4)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55200
%stackaddr$env-ref55201 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51265, i64 5)
store %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$env-ref55201
%stackaddr$prim55202 = alloca %struct.ScmObj*, align 8
%_95k47386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54089)
store volatile %struct.ScmObj* %_95k47386, %struct.ScmObj** %stackaddr$prim55202, align 8
%stackaddr$prim55203 = alloca %struct.ScmObj*, align 8
%current_45args54090 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54089)
store volatile %struct.ScmObj* %current_45args54090, %struct.ScmObj** %stackaddr$prim55203, align 8
%stackaddr$prim55204 = alloca %struct.ScmObj*, align 8
%anf_45bind47340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54090)
store volatile %struct.ScmObj* %anf_45bind47340, %struct.ScmObj** %stackaddr$prim55204, align 8
%stackaddr$makeclosure55205 = alloca %struct.ScmObj*, align 8
%fptrToInt55206 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51336 to i64
%ae51336 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55206)
store volatile %struct.ScmObj* %ae51336, %struct.ScmObj** %stackaddr$makeclosure55205, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51336, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51336, %struct.ScmObj* %anf_45bind47329, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51336, %struct.ScmObj* %anf_45bind47321, i64 2)
%argslist54100$anf_45bind473300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55207 = alloca %struct.ScmObj*, align 8
%argslist54100$anf_45bind473301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47340, %struct.ScmObj* %argslist54100$anf_45bind473300)
store volatile %struct.ScmObj* %argslist54100$anf_45bind473301, %struct.ScmObj** %stackaddr$prim55207, align 8
%stackaddr$prim55208 = alloca %struct.ScmObj*, align 8
%argslist54100$anf_45bind473302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47338, %struct.ScmObj* %argslist54100$anf_45bind473301)
store volatile %struct.ScmObj* %argslist54100$anf_45bind473302, %struct.ScmObj** %stackaddr$prim55208, align 8
%stackaddr$prim55209 = alloca %struct.ScmObj*, align 8
%argslist54100$anf_45bind473303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47335, %struct.ScmObj* %argslist54100$anf_45bind473302)
store volatile %struct.ScmObj* %argslist54100$anf_45bind473303, %struct.ScmObj** %stackaddr$prim55209, align 8
%stackaddr$prim55210 = alloca %struct.ScmObj*, align 8
%argslist54100$anf_45bind473304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51336, %struct.ScmObj* %argslist54100$anf_45bind473303)
store volatile %struct.ScmObj* %argslist54100$anf_45bind473304, %struct.ScmObj** %stackaddr$prim55210, align 8
%clofunc55211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47330)
musttail call tailcc void %clofunc55211(%struct.ScmObj* %anf_45bind47330, %struct.ScmObj* %argslist54100$anf_45bind473304)
ret void
}

define tailcc void @proc_clo$ae51336(%struct.ScmObj* %env$ae51336,%struct.ScmObj* %current_45args54092) {
%stackaddr$env-ref55212 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51336, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55212
%stackaddr$env-ref55213 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51336, i64 1)
store %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$env-ref55213
%stackaddr$env-ref55214 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51336, i64 2)
store %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$env-ref55214
%stackaddr$prim55215 = alloca %struct.ScmObj*, align 8
%_95k47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54092)
store volatile %struct.ScmObj* %_95k47387, %struct.ScmObj** %stackaddr$prim55215, align 8
%stackaddr$prim55216 = alloca %struct.ScmObj*, align 8
%current_45args54093 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54092)
store volatile %struct.ScmObj* %current_45args54093, %struct.ScmObj** %stackaddr$prim55216, align 8
%stackaddr$prim55217 = alloca %struct.ScmObj*, align 8
%anf_45bind47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54093)
store volatile %struct.ScmObj* %anf_45bind47341, %struct.ScmObj** %stackaddr$prim55217, align 8
%stackaddr$makeclosure55218 = alloca %struct.ScmObj*, align 8
%fptrToInt55219 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51341 to i64
%ae51341 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55219)
store volatile %struct.ScmObj* %ae51341, %struct.ScmObj** %stackaddr$makeclosure55218, align 8
%ae51343 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54099$_37foldl471720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55220 = alloca %struct.ScmObj*, align 8
%argslist54099$_37foldl471721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47341, %struct.ScmObj* %argslist54099$_37foldl471720)
store volatile %struct.ScmObj* %argslist54099$_37foldl471721, %struct.ScmObj** %stackaddr$prim55220, align 8
%stackaddr$prim55221 = alloca %struct.ScmObj*, align 8
%argslist54099$_37foldl471722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47329, %struct.ScmObj* %argslist54099$_37foldl471721)
store volatile %struct.ScmObj* %argslist54099$_37foldl471722, %struct.ScmObj** %stackaddr$prim55221, align 8
%stackaddr$prim55222 = alloca %struct.ScmObj*, align 8
%argslist54099$_37foldl471723 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51343, %struct.ScmObj* %argslist54099$_37foldl471722)
store volatile %struct.ScmObj* %argslist54099$_37foldl471723, %struct.ScmObj** %stackaddr$prim55222, align 8
%stackaddr$prim55223 = alloca %struct.ScmObj*, align 8
%argslist54099$_37foldl471724 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47321, %struct.ScmObj* %argslist54099$_37foldl471723)
store volatile %struct.ScmObj* %argslist54099$_37foldl471724, %struct.ScmObj** %stackaddr$prim55223, align 8
%stackaddr$prim55224 = alloca %struct.ScmObj*, align 8
%argslist54099$_37foldl471725 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51341, %struct.ScmObj* %argslist54099$_37foldl471724)
store volatile %struct.ScmObj* %argslist54099$_37foldl471725, %struct.ScmObj** %stackaddr$prim55224, align 8
%clofunc55225 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47172)
musttail call tailcc void %clofunc55225(%struct.ScmObj* %_37foldl47172, %struct.ScmObj* %argslist54099$_37foldl471725)
ret void
}

define tailcc void @proc_clo$ae51341(%struct.ScmObj* %env$ae51341,%struct.ScmObj* %current_45args54095) {
%stackaddr$prim55226 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54095)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55226, align 8
%stackaddr$prim55227 = alloca %struct.ScmObj*, align 8
%current_45args54096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54095)
store volatile %struct.ScmObj* %current_45args54096, %struct.ScmObj** %stackaddr$prim55227, align 8
%stackaddr$prim55228 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54096)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55228, align 8
%stackaddr$prim55229 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55229, align 8
%argslist54098$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55230 = alloca %struct.ScmObj*, align 8
%argslist54098$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54098$k0)
store volatile %struct.ScmObj* %argslist54098$k1, %struct.ScmObj** %stackaddr$prim55230, align 8
%clofunc55231 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55231(%struct.ScmObj* %k, %struct.ScmObj* %argslist54098$k1)
ret void
}

define tailcc void @proc_clo$ae51267(%struct.ScmObj* %env$ae51267,%struct.ScmObj* %a4720547388) {
%stackaddr$env-ref55232 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51267, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55232
%stackaddr$prim55233 = alloca %struct.ScmObj*, align 8
%k47389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %a4720547388)
store volatile %struct.ScmObj* %k47389, %struct.ScmObj** %stackaddr$prim55233, align 8
%stackaddr$prim55234 = alloca %struct.ScmObj*, align 8
%a47205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %a4720547388)
store volatile %struct.ScmObj* %a47205, %struct.ScmObj** %stackaddr$prim55234, align 8
%stackaddr$makeclosure55235 = alloca %struct.ScmObj*, align 8
%fptrToInt55236 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51270 to i64
%ae51270 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55236)
store volatile %struct.ScmObj* %ae51270, %struct.ScmObj** %stackaddr$makeclosure55235, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51270, %struct.ScmObj* %a47205, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51270, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51270, %struct.ScmObj* %k47389, i64 2)
%ae51271 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55237 = alloca %struct.ScmObj*, align 8
%fptrToInt55238 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51272 to i64
%ae51272 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55238)
store volatile %struct.ScmObj* %ae51272, %struct.ScmObj** %stackaddr$makeclosure55237, align 8
%argslist54106$ae512700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55239 = alloca %struct.ScmObj*, align 8
%argslist54106$ae512701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51272, %struct.ScmObj* %argslist54106$ae512700)
store volatile %struct.ScmObj* %argslist54106$ae512701, %struct.ScmObj** %stackaddr$prim55239, align 8
%stackaddr$prim55240 = alloca %struct.ScmObj*, align 8
%argslist54106$ae512702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51271, %struct.ScmObj* %argslist54106$ae512701)
store volatile %struct.ScmObj* %argslist54106$ae512702, %struct.ScmObj** %stackaddr$prim55240, align 8
%clofunc55241 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51270)
musttail call tailcc void %clofunc55241(%struct.ScmObj* %ae51270, %struct.ScmObj* %argslist54106$ae512702)
ret void
}

define tailcc void @proc_clo$ae51270(%struct.ScmObj* %env$ae51270,%struct.ScmObj* %current_45args54101) {
%stackaddr$env-ref55242 = alloca %struct.ScmObj*, align 8
%a47205 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51270, i64 0)
store %struct.ScmObj* %a47205, %struct.ScmObj** %stackaddr$env-ref55242
%stackaddr$env-ref55243 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51270, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55243
%stackaddr$env-ref55244 = alloca %struct.ScmObj*, align 8
%k47389 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51270, i64 2)
store %struct.ScmObj* %k47389, %struct.ScmObj** %stackaddr$env-ref55244
%stackaddr$prim55245 = alloca %struct.ScmObj*, align 8
%_95k47390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54101)
store volatile %struct.ScmObj* %_95k47390, %struct.ScmObj** %stackaddr$prim55245, align 8
%stackaddr$prim55246 = alloca %struct.ScmObj*, align 8
%current_45args54102 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54101)
store volatile %struct.ScmObj* %current_45args54102, %struct.ScmObj** %stackaddr$prim55246, align 8
%stackaddr$prim55247 = alloca %struct.ScmObj*, align 8
%anf_45bind47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54102)
store volatile %struct.ScmObj* %anf_45bind47339, %struct.ScmObj** %stackaddr$prim55247, align 8
%ae51297 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54104$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55248 = alloca %struct.ScmObj*, align 8
%argslist54104$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47205, %struct.ScmObj* %argslist54104$_37foldr1470900)
store volatile %struct.ScmObj* %argslist54104$_37foldr1470901, %struct.ScmObj** %stackaddr$prim55248, align 8
%stackaddr$prim55249 = alloca %struct.ScmObj*, align 8
%argslist54104$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51297, %struct.ScmObj* %argslist54104$_37foldr1470901)
store volatile %struct.ScmObj* %argslist54104$_37foldr1470902, %struct.ScmObj** %stackaddr$prim55249, align 8
%stackaddr$prim55250 = alloca %struct.ScmObj*, align 8
%argslist54104$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47339, %struct.ScmObj* %argslist54104$_37foldr1470902)
store volatile %struct.ScmObj* %argslist54104$_37foldr1470903, %struct.ScmObj** %stackaddr$prim55250, align 8
%stackaddr$prim55251 = alloca %struct.ScmObj*, align 8
%argslist54104$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47389, %struct.ScmObj* %argslist54104$_37foldr1470903)
store volatile %struct.ScmObj* %argslist54104$_37foldr1470904, %struct.ScmObj** %stackaddr$prim55251, align 8
%clofunc55252 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc55252(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist54104$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae51272(%struct.ScmObj* %env$ae51272,%struct.ScmObj* %args4720647391) {
%stackaddr$prim55253 = alloca %struct.ScmObj*, align 8
%k47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4720647391)
store volatile %struct.ScmObj* %k47392, %struct.ScmObj** %stackaddr$prim55253, align 8
%stackaddr$prim55254 = alloca %struct.ScmObj*, align 8
%args47206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4720647391)
store volatile %struct.ScmObj* %args47206, %struct.ScmObj** %stackaddr$prim55254, align 8
%stackaddr$applyprim55255 = alloca %struct.ScmObj*, align 8
%cpsaprim47393 = call %struct.ScmObj* @applyprim__43(%struct.ScmObj* %args47206)
store volatile %struct.ScmObj* %cpsaprim47393, %struct.ScmObj** %stackaddr$applyprim55255, align 8
%ae51277 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54105$k473920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55256 = alloca %struct.ScmObj*, align 8
%argslist54105$k473921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim47393, %struct.ScmObj* %argslist54105$k473920)
store volatile %struct.ScmObj* %argslist54105$k473921, %struct.ScmObj** %stackaddr$prim55256, align 8
%stackaddr$prim55257 = alloca %struct.ScmObj*, align 8
%argslist54105$k473922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51277, %struct.ScmObj* %argslist54105$k473921)
store volatile %struct.ScmObj* %argslist54105$k473922, %struct.ScmObj** %stackaddr$prim55257, align 8
%clofunc55258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47392)
musttail call tailcc void %clofunc55258(%struct.ScmObj* %k47392, %struct.ScmObj* %argslist54105$k473922)
ret void
}

define tailcc void @proc_clo$ae51237(%struct.ScmObj* %env$ae51237,%struct.ScmObj* %current_45args54108) {
%stackaddr$prim55259 = alloca %struct.ScmObj*, align 8
%k47394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54108)
store volatile %struct.ScmObj* %k47394, %struct.ScmObj** %stackaddr$prim55259, align 8
%stackaddr$prim55260 = alloca %struct.ScmObj*, align 8
%current_45args54109 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54108)
store volatile %struct.ScmObj* %current_45args54109, %struct.ScmObj** %stackaddr$prim55260, align 8
%stackaddr$prim55261 = alloca %struct.ScmObj*, align 8
%a47204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54109)
store volatile %struct.ScmObj* %a47204, %struct.ScmObj** %stackaddr$prim55261, align 8
%stackaddr$prim55262 = alloca %struct.ScmObj*, align 8
%current_45args54110 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54109)
store volatile %struct.ScmObj* %current_45args54110, %struct.ScmObj** %stackaddr$prim55262, align 8
%stackaddr$prim55263 = alloca %struct.ScmObj*, align 8
%b47203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54110)
store volatile %struct.ScmObj* %b47203, %struct.ScmObj** %stackaddr$prim55263, align 8
%stackaddr$prim55264 = alloca %struct.ScmObj*, align 8
%current_45args54111 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54110)
store volatile %struct.ScmObj* %current_45args54111, %struct.ScmObj** %stackaddr$prim55264, align 8
%stackaddr$prim55265 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54111)
store volatile %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$prim55265, align 8
%stackaddr$prim55266 = alloca %struct.ScmObj*, align 8
%current_45args54112 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54111)
store volatile %struct.ScmObj* %current_45args54112, %struct.ScmObj** %stackaddr$prim55266, align 8
%stackaddr$prim55267 = alloca %struct.ScmObj*, align 8
%d47201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54112)
store volatile %struct.ScmObj* %d47201, %struct.ScmObj** %stackaddr$prim55267, align 8
%stackaddr$prim55268 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %a47204, %struct.ScmObj* %b47203)
store volatile %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$prim55268, align 8
%stackaddr$prim55269 = alloca %struct.ScmObj*, align 8
%anf_45bind47337 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %c47202, %struct.ScmObj* %d47201)
store volatile %struct.ScmObj* %anf_45bind47337, %struct.ScmObj** %stackaddr$prim55269, align 8
%stackaddr$prim55270 = alloca %struct.ScmObj*, align 8
%cpsprim47395 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind47336, %struct.ScmObj* %anf_45bind47337)
store volatile %struct.ScmObj* %cpsprim47395, %struct.ScmObj** %stackaddr$prim55270, align 8
%ae51245 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54114$k473940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55271 = alloca %struct.ScmObj*, align 8
%argslist54114$k473941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47395, %struct.ScmObj* %argslist54114$k473940)
store volatile %struct.ScmObj* %argslist54114$k473941, %struct.ScmObj** %stackaddr$prim55271, align 8
%stackaddr$prim55272 = alloca %struct.ScmObj*, align 8
%argslist54114$k473942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51245, %struct.ScmObj* %argslist54114$k473941)
store volatile %struct.ScmObj* %argslist54114$k473942, %struct.ScmObj** %stackaddr$prim55272, align 8
%clofunc55273 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47394)
musttail call tailcc void %clofunc55273(%struct.ScmObj* %k47394, %struct.ScmObj* %argslist54114$k473942)
ret void
}

define tailcc void @proc_clo$ae51165(%struct.ScmObj* %env$ae51165,%struct.ScmObj* %t470684719547396) {
%stackaddr$env-ref55274 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51165, i64 0)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref55274
%stackaddr$env-ref55275 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51165, i64 1)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55275
%stackaddr$prim55276 = alloca %struct.ScmObj*, align 8
%k47397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t470684719547396)
store volatile %struct.ScmObj* %k47397, %struct.ScmObj** %stackaddr$prim55276, align 8
%stackaddr$prim55277 = alloca %struct.ScmObj*, align 8
%t4706847195 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t470684719547396)
store volatile %struct.ScmObj* %t4706847195, %struct.ScmObj** %stackaddr$prim55277, align 8
%stackaddr$prim55278 = alloca %struct.ScmObj*, align 8
%a47196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t4706847195)
store volatile %struct.ScmObj* %a47196, %struct.ScmObj** %stackaddr$prim55278, align 8
%stackaddr$prim55279 = alloca %struct.ScmObj*, align 8
%t4706847197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t4706847195)
store volatile %struct.ScmObj* %t4706847197, %struct.ScmObj** %stackaddr$prim55279, align 8
%stackaddr$prim55280 = alloca %struct.ScmObj*, align 8
%b47198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t4706847197)
store volatile %struct.ScmObj* %b47198, %struct.ScmObj** %stackaddr$prim55280, align 8
%stackaddr$prim55281 = alloca %struct.ScmObj*, align 8
%t4706847199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t4706847197)
store volatile %struct.ScmObj* %t4706847199, %struct.ScmObj** %stackaddr$prim55281, align 8
%stackaddr$makeclosure55282 = alloca %struct.ScmObj*, align 8
%fptrToInt55283 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51172 to i64
%ae51172 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55283)
store volatile %struct.ScmObj* %ae51172, %struct.ScmObj** %stackaddr$makeclosure55282, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51172, %struct.ScmObj* %a47196, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51172, %struct.ScmObj* %_37first47132, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51172, %struct.ScmObj* %k47397, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51172, %struct.ScmObj* %b47198, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51172, %struct.ScmObj* %_37second47130, i64 4)
%ae51173 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54128$ae511720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55284 = alloca %struct.ScmObj*, align 8
%argslist54128$ae511721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %t4706847199, %struct.ScmObj* %argslist54128$ae511720)
store volatile %struct.ScmObj* %argslist54128$ae511721, %struct.ScmObj** %stackaddr$prim55284, align 8
%stackaddr$prim55285 = alloca %struct.ScmObj*, align 8
%argslist54128$ae511722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51173, %struct.ScmObj* %argslist54128$ae511721)
store volatile %struct.ScmObj* %argslist54128$ae511722, %struct.ScmObj** %stackaddr$prim55285, align 8
%clofunc55286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51172)
musttail call tailcc void %clofunc55286(%struct.ScmObj* %ae51172, %struct.ScmObj* %argslist54128$ae511722)
ret void
}

define tailcc void @proc_clo$ae51172(%struct.ScmObj* %env$ae51172,%struct.ScmObj* %current_45args54116) {
%stackaddr$env-ref55287 = alloca %struct.ScmObj*, align 8
%a47196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51172, i64 0)
store %struct.ScmObj* %a47196, %struct.ScmObj** %stackaddr$env-ref55287
%stackaddr$env-ref55288 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51172, i64 1)
store %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$env-ref55288
%stackaddr$env-ref55289 = alloca %struct.ScmObj*, align 8
%k47397 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51172, i64 2)
store %struct.ScmObj* %k47397, %struct.ScmObj** %stackaddr$env-ref55289
%stackaddr$env-ref55290 = alloca %struct.ScmObj*, align 8
%b47198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51172, i64 3)
store %struct.ScmObj* %b47198, %struct.ScmObj** %stackaddr$env-ref55290
%stackaddr$env-ref55291 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51172, i64 4)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55291
%stackaddr$prim55292 = alloca %struct.ScmObj*, align 8
%_95k47398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54116)
store volatile %struct.ScmObj* %_95k47398, %struct.ScmObj** %stackaddr$prim55292, align 8
%stackaddr$prim55293 = alloca %struct.ScmObj*, align 8
%current_45args54117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54116)
store volatile %struct.ScmObj* %current_45args54117, %struct.ScmObj** %stackaddr$prim55293, align 8
%stackaddr$prim55294 = alloca %struct.ScmObj*, align 8
%c47200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54117)
store volatile %struct.ScmObj* %c47200, %struct.ScmObj** %stackaddr$prim55294, align 8
%stackaddr$makeclosure55295 = alloca %struct.ScmObj*, align 8
%fptrToInt55296 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51179 to i64
%ae51179 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55296)
store volatile %struct.ScmObj* %ae51179, %struct.ScmObj** %stackaddr$makeclosure55295, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51179, %struct.ScmObj* %k47397, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51179, %struct.ScmObj* %c47200, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51179, %struct.ScmObj* %b47198, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51179, %struct.ScmObj* %a47196, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51179, %struct.ScmObj* %_37second47130, i64 4)
%argslist54127$_37first471320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55297 = alloca %struct.ScmObj*, align 8
%argslist54127$_37first471321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c47200, %struct.ScmObj* %argslist54127$_37first471320)
store volatile %struct.ScmObj* %argslist54127$_37first471321, %struct.ScmObj** %stackaddr$prim55297, align 8
%stackaddr$prim55298 = alloca %struct.ScmObj*, align 8
%argslist54127$_37first471322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51179, %struct.ScmObj* %argslist54127$_37first471321)
store volatile %struct.ScmObj* %argslist54127$_37first471322, %struct.ScmObj** %stackaddr$prim55298, align 8
%clofunc55299 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37first47132)
musttail call tailcc void %clofunc55299(%struct.ScmObj* %_37first47132, %struct.ScmObj* %argslist54127$_37first471322)
ret void
}

define tailcc void @proc_clo$ae51179(%struct.ScmObj* %env$ae51179,%struct.ScmObj* %current_45args54119) {
%stackaddr$env-ref55300 = alloca %struct.ScmObj*, align 8
%k47397 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51179, i64 0)
store %struct.ScmObj* %k47397, %struct.ScmObj** %stackaddr$env-ref55300
%stackaddr$env-ref55301 = alloca %struct.ScmObj*, align 8
%c47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51179, i64 1)
store %struct.ScmObj* %c47200, %struct.ScmObj** %stackaddr$env-ref55301
%stackaddr$env-ref55302 = alloca %struct.ScmObj*, align 8
%b47198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51179, i64 2)
store %struct.ScmObj* %b47198, %struct.ScmObj** %stackaddr$env-ref55302
%stackaddr$env-ref55303 = alloca %struct.ScmObj*, align 8
%a47196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51179, i64 3)
store %struct.ScmObj* %a47196, %struct.ScmObj** %stackaddr$env-ref55303
%stackaddr$env-ref55304 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51179, i64 4)
store %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$env-ref55304
%stackaddr$prim55305 = alloca %struct.ScmObj*, align 8
%_95k47399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54119)
store volatile %struct.ScmObj* %_95k47399, %struct.ScmObj** %stackaddr$prim55305, align 8
%stackaddr$prim55306 = alloca %struct.ScmObj*, align 8
%current_45args54120 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54119)
store volatile %struct.ScmObj* %current_45args54120, %struct.ScmObj** %stackaddr$prim55306, align 8
%stackaddr$prim55307 = alloca %struct.ScmObj*, align 8
%anf_45bind47331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54120)
store volatile %struct.ScmObj* %anf_45bind47331, %struct.ScmObj** %stackaddr$prim55307, align 8
%stackaddr$makeclosure55308 = alloca %struct.ScmObj*, align 8
%fptrToInt55309 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51182 to i64
%ae51182 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55309)
store volatile %struct.ScmObj* %ae51182, %struct.ScmObj** %stackaddr$makeclosure55308, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51182, %struct.ScmObj* %k47397, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51182, %struct.ScmObj* %anf_45bind47331, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51182, %struct.ScmObj* %b47198, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51182, %struct.ScmObj* %a47196, i64 3)
%argslist54126$_37second471300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55310 = alloca %struct.ScmObj*, align 8
%argslist54126$_37second471301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c47200, %struct.ScmObj* %argslist54126$_37second471300)
store volatile %struct.ScmObj* %argslist54126$_37second471301, %struct.ScmObj** %stackaddr$prim55310, align 8
%stackaddr$prim55311 = alloca %struct.ScmObj*, align 8
%argslist54126$_37second471302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51182, %struct.ScmObj* %argslist54126$_37second471301)
store volatile %struct.ScmObj* %argslist54126$_37second471302, %struct.ScmObj** %stackaddr$prim55311, align 8
%clofunc55312 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37second47130)
musttail call tailcc void %clofunc55312(%struct.ScmObj* %_37second47130, %struct.ScmObj* %argslist54126$_37second471302)
ret void
}

define tailcc void @proc_clo$ae51182(%struct.ScmObj* %env$ae51182,%struct.ScmObj* %current_45args54122) {
%stackaddr$env-ref55313 = alloca %struct.ScmObj*, align 8
%k47397 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51182, i64 0)
store %struct.ScmObj* %k47397, %struct.ScmObj** %stackaddr$env-ref55313
%stackaddr$env-ref55314 = alloca %struct.ScmObj*, align 8
%anf_45bind47331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51182, i64 1)
store %struct.ScmObj* %anf_45bind47331, %struct.ScmObj** %stackaddr$env-ref55314
%stackaddr$env-ref55315 = alloca %struct.ScmObj*, align 8
%b47198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51182, i64 2)
store %struct.ScmObj* %b47198, %struct.ScmObj** %stackaddr$env-ref55315
%stackaddr$env-ref55316 = alloca %struct.ScmObj*, align 8
%a47196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51182, i64 3)
store %struct.ScmObj* %a47196, %struct.ScmObj** %stackaddr$env-ref55316
%stackaddr$prim55317 = alloca %struct.ScmObj*, align 8
%_95k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54122)
store volatile %struct.ScmObj* %_95k47400, %struct.ScmObj** %stackaddr$prim55317, align 8
%stackaddr$prim55318 = alloca %struct.ScmObj*, align 8
%current_45args54123 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54122)
store volatile %struct.ScmObj* %current_45args54123, %struct.ScmObj** %stackaddr$prim55318, align 8
%stackaddr$prim55319 = alloca %struct.ScmObj*, align 8
%anf_45bind47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54123)
store volatile %struct.ScmObj* %anf_45bind47332, %struct.ScmObj** %stackaddr$prim55319, align 8
%stackaddr$prim55320 = alloca %struct.ScmObj*, align 8
%anf_45bind47333 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind47331, %struct.ScmObj* %anf_45bind47332)
store volatile %struct.ScmObj* %anf_45bind47333, %struct.ScmObj** %stackaddr$prim55320, align 8
%stackaddr$prim55321 = alloca %struct.ScmObj*, align 8
%anf_45bind47334 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %b47198, %struct.ScmObj* %anf_45bind47333)
store volatile %struct.ScmObj* %anf_45bind47334, %struct.ScmObj** %stackaddr$prim55321, align 8
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%cpsprim47401 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %a47196, %struct.ScmObj* %anf_45bind47334)
store volatile %struct.ScmObj* %cpsprim47401, %struct.ScmObj** %stackaddr$prim55322, align 8
%ae51191 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54125$k473970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55323 = alloca %struct.ScmObj*, align 8
%argslist54125$k473971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47401, %struct.ScmObj* %argslist54125$k473970)
store volatile %struct.ScmObj* %argslist54125$k473971, %struct.ScmObj** %stackaddr$prim55323, align 8
%stackaddr$prim55324 = alloca %struct.ScmObj*, align 8
%argslist54125$k473972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51191, %struct.ScmObj* %argslist54125$k473971)
store volatile %struct.ScmObj* %argslist54125$k473972, %struct.ScmObj** %stackaddr$prim55324, align 8
%clofunc55325 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47397)
musttail call tailcc void %clofunc55325(%struct.ScmObj* %k47397, %struct.ScmObj* %argslist54125$k473972)
ret void
}

define tailcc void @proc_clo$ae51143(%struct.ScmObj* %env$ae51143,%struct.ScmObj* %a4719447402) {
%stackaddr$prim55326 = alloca %struct.ScmObj*, align 8
%k47403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %a4719447402)
store volatile %struct.ScmObj* %k47403, %struct.ScmObj** %stackaddr$prim55326, align 8
%stackaddr$prim55327 = alloca %struct.ScmObj*, align 8
%a47194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %a4719447402)
store volatile %struct.ScmObj* %a47194, %struct.ScmObj** %stackaddr$prim55327, align 8
%ae51147 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54130$k474030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55328 = alloca %struct.ScmObj*, align 8
%argslist54130$k474031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47194, %struct.ScmObj* %argslist54130$k474030)
store volatile %struct.ScmObj* %argslist54130$k474031, %struct.ScmObj** %stackaddr$prim55328, align 8
%stackaddr$prim55329 = alloca %struct.ScmObj*, align 8
%argslist54130$k474032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51147, %struct.ScmObj* %argslist54130$k474031)
store volatile %struct.ScmObj* %argslist54130$k474032, %struct.ScmObj** %stackaddr$prim55329, align 8
%clofunc55330 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47403)
musttail call tailcc void %clofunc55330(%struct.ScmObj* %k47403, %struct.ScmObj* %argslist54130$k474032)
ret void
}

define tailcc void @proc_clo$ae51086(%struct.ScmObj* %env$ae51086,%struct.ScmObj* %lst4719347404) {
%stackaddr$prim55331 = alloca %struct.ScmObj*, align 8
%k47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4719347404)
store volatile %struct.ScmObj* %k47405, %struct.ScmObj** %stackaddr$prim55331, align 8
%stackaddr$prim55332 = alloca %struct.ScmObj*, align 8
%lst47193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4719347404)
store volatile %struct.ScmObj* %lst47193, %struct.ScmObj** %stackaddr$prim55332, align 8
%ae51090 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54134$k474050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55333 = alloca %struct.ScmObj*, align 8
%argslist54134$k474051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47193, %struct.ScmObj* %argslist54134$k474050)
store volatile %struct.ScmObj* %argslist54134$k474051, %struct.ScmObj** %stackaddr$prim55333, align 8
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%argslist54134$k474052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51090, %struct.ScmObj* %argslist54134$k474051)
store volatile %struct.ScmObj* %argslist54134$k474052, %struct.ScmObj** %stackaddr$prim55334, align 8
%clofunc55335 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47405)
musttail call tailcc void %clofunc55335(%struct.ScmObj* %k47405, %struct.ScmObj* %argslist54134$k474052)
ret void
}

define tailcc void @proc_clo$ae51034(%struct.ScmObj* %env$ae51034,%struct.ScmObj* %lst4719247406) {
%stackaddr$prim55336 = alloca %struct.ScmObj*, align 8
%k47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4719247406)
store volatile %struct.ScmObj* %k47407, %struct.ScmObj** %stackaddr$prim55336, align 8
%stackaddr$prim55337 = alloca %struct.ScmObj*, align 8
%lst47192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4719247406)
store volatile %struct.ScmObj* %lst47192, %struct.ScmObj** %stackaddr$prim55337, align 8
%ae51038 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54137$k474070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55338 = alloca %struct.ScmObj*, align 8
%argslist54137$k474071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47192, %struct.ScmObj* %argslist54137$k474070)
store volatile %struct.ScmObj* %argslist54137$k474071, %struct.ScmObj** %stackaddr$prim55338, align 8
%stackaddr$prim55339 = alloca %struct.ScmObj*, align 8
%argslist54137$k474072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51038, %struct.ScmObj* %argslist54137$k474071)
store volatile %struct.ScmObj* %argslist54137$k474072, %struct.ScmObj** %stackaddr$prim55339, align 8
%clofunc55340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47407)
musttail call tailcc void %clofunc55340(%struct.ScmObj* %k47407, %struct.ScmObj* %argslist54137$k474072)
ret void
}

define tailcc void @proc_clo$ae50982(%struct.ScmObj* %env$ae50982,%struct.ScmObj* %lst4719147408) {
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%k47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4719147408)
store volatile %struct.ScmObj* %k47409, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%lst47191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4719147408)
store volatile %struct.ScmObj* %lst47191, %struct.ScmObj** %stackaddr$prim55342, align 8
%ae50986 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54140$k474090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55343 = alloca %struct.ScmObj*, align 8
%argslist54140$k474091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47191, %struct.ScmObj* %argslist54140$k474090)
store volatile %struct.ScmObj* %argslist54140$k474091, %struct.ScmObj** %stackaddr$prim55343, align 8
%stackaddr$prim55344 = alloca %struct.ScmObj*, align 8
%argslist54140$k474092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50986, %struct.ScmObj* %argslist54140$k474091)
store volatile %struct.ScmObj* %argslist54140$k474092, %struct.ScmObj** %stackaddr$prim55344, align 8
%clofunc55345 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47409)
musttail call tailcc void %clofunc55345(%struct.ScmObj* %k47409, %struct.ScmObj* %argslist54140$k474092)
ret void
}

define tailcc void @proc_clo$ae50960(%struct.ScmObj* %env$ae50960,%struct.ScmObj* %lst4719047410) {
%stackaddr$prim55346 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4719047410)
store volatile %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$prim55346, align 8
%stackaddr$prim55347 = alloca %struct.ScmObj*, align 8
%lst47190 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4719047410)
store volatile %struct.ScmObj* %lst47190, %struct.ScmObj** %stackaddr$prim55347, align 8
%ae50964 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54142$k474110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55348 = alloca %struct.ScmObj*, align 8
%argslist54142$k474111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47190, %struct.ScmObj* %argslist54142$k474110)
store volatile %struct.ScmObj* %argslist54142$k474111, %struct.ScmObj** %stackaddr$prim55348, align 8
%stackaddr$prim55349 = alloca %struct.ScmObj*, align 8
%argslist54142$k474112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50964, %struct.ScmObj* %argslist54142$k474111)
store volatile %struct.ScmObj* %argslist54142$k474112, %struct.ScmObj** %stackaddr$prim55349, align 8
%clofunc55350 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47411)
musttail call tailcc void %clofunc55350(%struct.ScmObj* %k47411, %struct.ScmObj* %argslist54142$k474112)
ret void
}

define tailcc void @proc_clo$ae50922(%struct.ScmObj* %env$ae50922,%struct.ScmObj* %current_45args54144) {
%stackaddr$prim55351 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54144)
store volatile %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$prim55351, align 8
%stackaddr$prim55352 = alloca %struct.ScmObj*, align 8
%current_45args54145 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54144)
store volatile %struct.ScmObj* %current_45args54145, %struct.ScmObj** %stackaddr$prim55352, align 8
%stackaddr$prim55353 = alloca %struct.ScmObj*, align 8
%lst47189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54145)
store volatile %struct.ScmObj* %lst47189, %struct.ScmObj** %stackaddr$prim55353, align 8
%stackaddr$prim55354 = alloca %struct.ScmObj*, align 8
%current_45args54146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54145)
store volatile %struct.ScmObj* %current_45args54146, %struct.ScmObj** %stackaddr$prim55354, align 8
%stackaddr$prim55355 = alloca %struct.ScmObj*, align 8
%f47188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54146)
store volatile %struct.ScmObj* %f47188, %struct.ScmObj** %stackaddr$prim55355, align 8
%stackaddr$prim55356 = alloca %struct.ScmObj*, align 8
%current_45args54147 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54146)
store volatile %struct.ScmObj* %current_45args54147, %struct.ScmObj** %stackaddr$prim55356, align 8
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%a47187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54147)
store volatile %struct.ScmObj* %a47187, %struct.ScmObj** %stackaddr$prim55357, align 8
%stackaddr$makeclosure55358 = alloca %struct.ScmObj*, align 8
%fptrToInt55359 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50923 to i64
%ae50923 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55359)
store volatile %struct.ScmObj* %ae50923, %struct.ScmObj** %stackaddr$makeclosure55358, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50923, %struct.ScmObj* %lst47189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50923, %struct.ScmObj* %k47412, i64 1)
%stackaddr$prim55360 = alloca %struct.ScmObj*, align 8
%cpsargs47415 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50923, %struct.ScmObj* %lst47189)
store volatile %struct.ScmObj* %cpsargs47415, %struct.ScmObj** %stackaddr$prim55360, align 8
%clofunc55361 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47188)
musttail call tailcc void %clofunc55361(%struct.ScmObj* %f47188, %struct.ScmObj* %cpsargs47415)
ret void
}

define tailcc void @proc_clo$ae50923(%struct.ScmObj* %env$ae50923,%struct.ScmObj* %current_45args54149) {
%stackaddr$env-ref55362 = alloca %struct.ScmObj*, align 8
%lst47189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50923, i64 0)
store %struct.ScmObj* %lst47189, %struct.ScmObj** %stackaddr$env-ref55362
%stackaddr$env-ref55363 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50923, i64 1)
store %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$env-ref55363
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%_95k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54149)
store volatile %struct.ScmObj* %_95k47413, %struct.ScmObj** %stackaddr$prim55364, align 8
%stackaddr$prim55365 = alloca %struct.ScmObj*, align 8
%current_45args54150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54149)
store volatile %struct.ScmObj* %current_45args54150, %struct.ScmObj** %stackaddr$prim55365, align 8
%stackaddr$prim55366 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54150)
store volatile %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$prim55366, align 8
%stackaddr$prim55367 = alloca %struct.ScmObj*, align 8
%anf_45bind47319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47189)
store volatile %struct.ScmObj* %anf_45bind47319, %struct.ScmObj** %stackaddr$prim55367, align 8
%stackaddr$prim55368 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47319)
store volatile %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$prim55368, align 8
%stackaddr$prim55369 = alloca %struct.ScmObj*, align 8
%cpsprim47414 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind47318, %struct.ScmObj* %anf_45bind47320)
store volatile %struct.ScmObj* %cpsprim47414, %struct.ScmObj** %stackaddr$prim55369, align 8
%ae50932 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54152$k474120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55370 = alloca %struct.ScmObj*, align 8
%argslist54152$k474121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47414, %struct.ScmObj* %argslist54152$k474120)
store volatile %struct.ScmObj* %argslist54152$k474121, %struct.ScmObj** %stackaddr$prim55370, align 8
%stackaddr$prim55371 = alloca %struct.ScmObj*, align 8
%argslist54152$k474122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50932, %struct.ScmObj* %argslist54152$k474121)
store volatile %struct.ScmObj* %argslist54152$k474122, %struct.ScmObj** %stackaddr$prim55371, align 8
%clofunc55372 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47412)
musttail call tailcc void %clofunc55372(%struct.ScmObj* %k47412, %struct.ScmObj* %argslist54152$k474122)
ret void
}

define tailcc void @proc_clo$ae50896(%struct.ScmObj* %env$ae50896,%struct.ScmObj* %current_45args54154) {
%stackaddr$prim55373 = alloca %struct.ScmObj*, align 8
%k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54154)
store volatile %struct.ScmObj* %k47416, %struct.ScmObj** %stackaddr$prim55373, align 8
%stackaddr$prim55374 = alloca %struct.ScmObj*, align 8
%current_45args54155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54154)
store volatile %struct.ScmObj* %current_45args54155, %struct.ScmObj** %stackaddr$prim55374, align 8
%stackaddr$prim55375 = alloca %struct.ScmObj*, align 8
%x47127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54155)
store volatile %struct.ScmObj* %x47127, %struct.ScmObj** %stackaddr$prim55375, align 8
%stackaddr$prim55376 = alloca %struct.ScmObj*, align 8
%anf_45bind47315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47127)
store volatile %struct.ScmObj* %anf_45bind47315, %struct.ScmObj** %stackaddr$prim55376, align 8
%stackaddr$prim55377 = alloca %struct.ScmObj*, align 8
%anf_45bind47316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47315)
store volatile %struct.ScmObj* %anf_45bind47316, %struct.ScmObj** %stackaddr$prim55377, align 8
%stackaddr$prim55378 = alloca %struct.ScmObj*, align 8
%anf_45bind47317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47316)
store volatile %struct.ScmObj* %anf_45bind47317, %struct.ScmObj** %stackaddr$prim55378, align 8
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%cpsprim47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47317)
store volatile %struct.ScmObj* %cpsprim47417, %struct.ScmObj** %stackaddr$prim55379, align 8
%ae50902 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54157$k474160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55380 = alloca %struct.ScmObj*, align 8
%argslist54157$k474161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47417, %struct.ScmObj* %argslist54157$k474160)
store volatile %struct.ScmObj* %argslist54157$k474161, %struct.ScmObj** %stackaddr$prim55380, align 8
%stackaddr$prim55381 = alloca %struct.ScmObj*, align 8
%argslist54157$k474162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50902, %struct.ScmObj* %argslist54157$k474161)
store volatile %struct.ScmObj* %argslist54157$k474162, %struct.ScmObj** %stackaddr$prim55381, align 8
%clofunc55382 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47416)
musttail call tailcc void %clofunc55382(%struct.ScmObj* %k47416, %struct.ScmObj* %argslist54157$k474162)
ret void
}

define tailcc void @proc_clo$ae50872(%struct.ScmObj* %env$ae50872,%struct.ScmObj* %current_45args54159) {
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%k47418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54159)
store volatile %struct.ScmObj* %k47418, %struct.ScmObj** %stackaddr$prim55383, align 8
%stackaddr$prim55384 = alloca %struct.ScmObj*, align 8
%current_45args54160 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54159)
store volatile %struct.ScmObj* %current_45args54160, %struct.ScmObj** %stackaddr$prim55384, align 8
%stackaddr$prim55385 = alloca %struct.ScmObj*, align 8
%x47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54160)
store volatile %struct.ScmObj* %x47129, %struct.ScmObj** %stackaddr$prim55385, align 8
%stackaddr$prim55386 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47129)
store volatile %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$prim55386, align 8
%stackaddr$prim55387 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47313)
store volatile %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$prim55387, align 8
%stackaddr$prim55388 = alloca %struct.ScmObj*, align 8
%cpsprim47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47314)
store volatile %struct.ScmObj* %cpsprim47419, %struct.ScmObj** %stackaddr$prim55388, align 8
%ae50877 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54162$k474180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55389 = alloca %struct.ScmObj*, align 8
%argslist54162$k474181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47419, %struct.ScmObj* %argslist54162$k474180)
store volatile %struct.ScmObj* %argslist54162$k474181, %struct.ScmObj** %stackaddr$prim55389, align 8
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%argslist54162$k474182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50877, %struct.ScmObj* %argslist54162$k474181)
store volatile %struct.ScmObj* %argslist54162$k474182, %struct.ScmObj** %stackaddr$prim55390, align 8
%clofunc55391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47418)
musttail call tailcc void %clofunc55391(%struct.ScmObj* %k47418, %struct.ScmObj* %argslist54162$k474182)
ret void
}

define tailcc void @proc_clo$ae50850(%struct.ScmObj* %env$ae50850,%struct.ScmObj* %current_45args54164) {
%stackaddr$prim55392 = alloca %struct.ScmObj*, align 8
%k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54164)
store volatile %struct.ScmObj* %k47420, %struct.ScmObj** %stackaddr$prim55392, align 8
%stackaddr$prim55393 = alloca %struct.ScmObj*, align 8
%current_45args54165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54164)
store volatile %struct.ScmObj* %current_45args54165, %struct.ScmObj** %stackaddr$prim55393, align 8
%stackaddr$prim55394 = alloca %struct.ScmObj*, align 8
%x47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54165)
store volatile %struct.ScmObj* %x47131, %struct.ScmObj** %stackaddr$prim55394, align 8
%stackaddr$prim55395 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47131)
store volatile %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$prim55395, align 8
%stackaddr$prim55396 = alloca %struct.ScmObj*, align 8
%cpsprim47421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47312)
store volatile %struct.ScmObj* %cpsprim47421, %struct.ScmObj** %stackaddr$prim55396, align 8
%ae50854 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54167$k474200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55397 = alloca %struct.ScmObj*, align 8
%argslist54167$k474201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47421, %struct.ScmObj* %argslist54167$k474200)
store volatile %struct.ScmObj* %argslist54167$k474201, %struct.ScmObj** %stackaddr$prim55397, align 8
%stackaddr$prim55398 = alloca %struct.ScmObj*, align 8
%argslist54167$k474202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50854, %struct.ScmObj* %argslist54167$k474201)
store volatile %struct.ScmObj* %argslist54167$k474202, %struct.ScmObj** %stackaddr$prim55398, align 8
%clofunc55399 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47420)
musttail call tailcc void %clofunc55399(%struct.ScmObj* %k47420, %struct.ScmObj* %argslist54167$k474202)
ret void
}

define tailcc void @proc_clo$ae50830(%struct.ScmObj* %env$ae50830,%struct.ScmObj* %current_45args54169) {
%stackaddr$prim55400 = alloca %struct.ScmObj*, align 8
%k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %k47422, %struct.ScmObj** %stackaddr$prim55400, align 8
%stackaddr$prim55401 = alloca %struct.ScmObj*, align 8
%current_45args54170 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %current_45args54170, %struct.ScmObj** %stackaddr$prim55401, align 8
%stackaddr$prim55402 = alloca %struct.ScmObj*, align 8
%x47133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54170)
store volatile %struct.ScmObj* %x47133, %struct.ScmObj** %stackaddr$prim55402, align 8
%stackaddr$prim55403 = alloca %struct.ScmObj*, align 8
%cpsprim47423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47133)
store volatile %struct.ScmObj* %cpsprim47423, %struct.ScmObj** %stackaddr$prim55403, align 8
%ae50833 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54172$k474220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55404 = alloca %struct.ScmObj*, align 8
%argslist54172$k474221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47423, %struct.ScmObj* %argslist54172$k474220)
store volatile %struct.ScmObj* %argslist54172$k474221, %struct.ScmObj** %stackaddr$prim55404, align 8
%stackaddr$prim55405 = alloca %struct.ScmObj*, align 8
%argslist54172$k474222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50833, %struct.ScmObj* %argslist54172$k474221)
store volatile %struct.ScmObj* %argslist54172$k474222, %struct.ScmObj** %stackaddr$prim55405, align 8
%clofunc55406 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47422)
musttail call tailcc void %clofunc55406(%struct.ScmObj* %k47422, %struct.ScmObj* %argslist54172$k474222)
ret void
}

define tailcc void @proc_clo$ae50732(%struct.ScmObj* %env$ae50732,%struct.ScmObj* %args4713547424) {
%stackaddr$env-ref55407 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50732, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55407
%stackaddr$prim55408 = alloca %struct.ScmObj*, align 8
%k47425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4713547424)
store volatile %struct.ScmObj* %k47425, %struct.ScmObj** %stackaddr$prim55408, align 8
%stackaddr$prim55409 = alloca %struct.ScmObj*, align 8
%args47135 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4713547424)
store volatile %struct.ScmObj* %args47135, %struct.ScmObj** %stackaddr$prim55409, align 8
%stackaddr$prim55410 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim55410, align 8
%truthy$cmp55411 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47306)
%cmp$cmp55411 = icmp eq i64 %truthy$cmp55411, 1
br i1 %cmp$cmp55411, label %truebranch$cmp55411, label %falsebranch$cmp55411
truebranch$cmp55411:
%ae50738 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50739 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54174$k474250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55412 = alloca %struct.ScmObj*, align 8
%argslist54174$k474251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50739, %struct.ScmObj* %argslist54174$k474250)
store volatile %struct.ScmObj* %argslist54174$k474251, %struct.ScmObj** %stackaddr$prim55412, align 8
%stackaddr$prim55413 = alloca %struct.ScmObj*, align 8
%argslist54174$k474252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50738, %struct.ScmObj* %argslist54174$k474251)
store volatile %struct.ScmObj* %argslist54174$k474252, %struct.ScmObj** %stackaddr$prim55413, align 8
%clofunc55414 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47425)
musttail call tailcc void %clofunc55414(%struct.ScmObj* %k47425, %struct.ScmObj* %argslist54174$k474252)
ret void
falsebranch$cmp55411:
%stackaddr$prim55415 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim55415, align 8
%stackaddr$prim55416 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47307)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim55416, align 8
%truthy$cmp55417 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47308)
%cmp$cmp55417 = icmp eq i64 %truthy$cmp55417, 1
br i1 %cmp$cmp55417, label %truebranch$cmp55417, label %falsebranch$cmp55417
truebranch$cmp55417:
%stackaddr$prim55418 = alloca %struct.ScmObj*, align 8
%cpsprim47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %cpsprim47426, %struct.ScmObj** %stackaddr$prim55418, align 8
%ae50751 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54175$k474250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55419 = alloca %struct.ScmObj*, align 8
%argslist54175$k474251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47426, %struct.ScmObj* %argslist54175$k474250)
store volatile %struct.ScmObj* %argslist54175$k474251, %struct.ScmObj** %stackaddr$prim55419, align 8
%stackaddr$prim55420 = alloca %struct.ScmObj*, align 8
%argslist54175$k474252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50751, %struct.ScmObj* %argslist54175$k474251)
store volatile %struct.ScmObj* %argslist54175$k474252, %struct.ScmObj** %stackaddr$prim55420, align 8
%clofunc55421 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47425)
musttail call tailcc void %clofunc55421(%struct.ScmObj* %k47425, %struct.ScmObj* %argslist54175$k474252)
ret void
falsebranch$cmp55417:
%stackaddr$makeclosure55422 = alloca %struct.ScmObj*, align 8
%fptrToInt55423 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50756 to i64
%ae50756 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55423)
store volatile %struct.ScmObj* %ae50756, %struct.ScmObj** %stackaddr$makeclosure55422, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50756, %struct.ScmObj* %_37foldl147074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50756, %struct.ScmObj* %k47425, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50756, %struct.ScmObj* %args47135, i64 2)
%ae50757 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55424 = alloca %struct.ScmObj*, align 8
%fptrToInt55425 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50758 to i64
%ae50758 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55425)
store volatile %struct.ScmObj* %ae50758, %struct.ScmObj** %stackaddr$makeclosure55424, align 8
%argslist54185$ae507560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%argslist54185$ae507561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50758, %struct.ScmObj* %argslist54185$ae507560)
store volatile %struct.ScmObj* %argslist54185$ae507561, %struct.ScmObj** %stackaddr$prim55426, align 8
%stackaddr$prim55427 = alloca %struct.ScmObj*, align 8
%argslist54185$ae507562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50757, %struct.ScmObj* %argslist54185$ae507561)
store volatile %struct.ScmObj* %argslist54185$ae507562, %struct.ScmObj** %stackaddr$prim55427, align 8
%clofunc55428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50756)
musttail call tailcc void %clofunc55428(%struct.ScmObj* %ae50756, %struct.ScmObj* %argslist54185$ae507562)
ret void
}

define tailcc void @proc_clo$ae50756(%struct.ScmObj* %env$ae50756,%struct.ScmObj* %current_45args54176) {
%stackaddr$env-ref55429 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50756, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55429
%stackaddr$env-ref55430 = alloca %struct.ScmObj*, align 8
%k47425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50756, i64 1)
store %struct.ScmObj* %k47425, %struct.ScmObj** %stackaddr$env-ref55430
%stackaddr$env-ref55431 = alloca %struct.ScmObj*, align 8
%args47135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50756, i64 2)
store %struct.ScmObj* %args47135, %struct.ScmObj** %stackaddr$env-ref55431
%stackaddr$prim55432 = alloca %struct.ScmObj*, align 8
%_95k47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54176)
store volatile %struct.ScmObj* %_95k47427, %struct.ScmObj** %stackaddr$prim55432, align 8
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%current_45args54177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54176)
store volatile %struct.ScmObj* %current_45args54177, %struct.ScmObj** %stackaddr$prim55433, align 8
%stackaddr$prim55434 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54177)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim55434, align 8
%stackaddr$prim55435 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim55435, align 8
%stackaddr$prim55436 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$prim55436, align 8
%argslist54179$_37foldl1470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55437 = alloca %struct.ScmObj*, align 8
%argslist54179$_37foldl1470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47311, %struct.ScmObj* %argslist54179$_37foldl1470740)
store volatile %struct.ScmObj* %argslist54179$_37foldl1470741, %struct.ScmObj** %stackaddr$prim55437, align 8
%stackaddr$prim55438 = alloca %struct.ScmObj*, align 8
%argslist54179$_37foldl1470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47310, %struct.ScmObj* %argslist54179$_37foldl1470741)
store volatile %struct.ScmObj* %argslist54179$_37foldl1470742, %struct.ScmObj** %stackaddr$prim55438, align 8
%stackaddr$prim55439 = alloca %struct.ScmObj*, align 8
%argslist54179$_37foldl1470743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47309, %struct.ScmObj* %argslist54179$_37foldl1470742)
store volatile %struct.ScmObj* %argslist54179$_37foldl1470743, %struct.ScmObj** %stackaddr$prim55439, align 8
%stackaddr$prim55440 = alloca %struct.ScmObj*, align 8
%argslist54179$_37foldl1470744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47425, %struct.ScmObj* %argslist54179$_37foldl1470743)
store volatile %struct.ScmObj* %argslist54179$_37foldl1470744, %struct.ScmObj** %stackaddr$prim55440, align 8
%clofunc55441 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147074)
musttail call tailcc void %clofunc55441(%struct.ScmObj* %_37foldl147074, %struct.ScmObj* %argslist54179$_37foldl1470744)
ret void
}

define tailcc void @proc_clo$ae50758(%struct.ScmObj* %env$ae50758,%struct.ScmObj* %current_45args54180) {
%stackaddr$prim55442 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54180)
store volatile %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$prim55442, align 8
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%current_45args54181 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54180)
store volatile %struct.ScmObj* %current_45args54181, %struct.ScmObj** %stackaddr$prim55443, align 8
%stackaddr$prim55444 = alloca %struct.ScmObj*, align 8
%n47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54181)
store volatile %struct.ScmObj* %n47137, %struct.ScmObj** %stackaddr$prim55444, align 8
%stackaddr$prim55445 = alloca %struct.ScmObj*, align 8
%current_45args54182 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54181)
store volatile %struct.ScmObj* %current_45args54182, %struct.ScmObj** %stackaddr$prim55445, align 8
%stackaddr$prim55446 = alloca %struct.ScmObj*, align 8
%v47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54182)
store volatile %struct.ScmObj* %v47136, %struct.ScmObj** %stackaddr$prim55446, align 8
%stackaddr$prim55447 = alloca %struct.ScmObj*, align 8
%cpsprim47429 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47136, %struct.ScmObj* %n47137)
store volatile %struct.ScmObj* %cpsprim47429, %struct.ScmObj** %stackaddr$prim55447, align 8
%ae50762 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54184$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55448 = alloca %struct.ScmObj*, align 8
%argslist54184$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47429, %struct.ScmObj* %argslist54184$k474280)
store volatile %struct.ScmObj* %argslist54184$k474281, %struct.ScmObj** %stackaddr$prim55448, align 8
%stackaddr$prim55449 = alloca %struct.ScmObj*, align 8
%argslist54184$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50762, %struct.ScmObj* %argslist54184$k474281)
store volatile %struct.ScmObj* %argslist54184$k474282, %struct.ScmObj** %stackaddr$prim55449, align 8
%clofunc55450 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc55450(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist54184$k474282)
ret void
}

define tailcc void @proc_clo$ae50328(%struct.ScmObj* %env$ae50328,%struct.ScmObj* %current_45args54187) {
%stackaddr$prim55451 = alloca %struct.ScmObj*, align 8
%k47430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54187)
store volatile %struct.ScmObj* %k47430, %struct.ScmObj** %stackaddr$prim55451, align 8
%stackaddr$prim55452 = alloca %struct.ScmObj*, align 8
%current_45args54188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54187)
store volatile %struct.ScmObj* %current_45args54188, %struct.ScmObj** %stackaddr$prim55452, align 8
%stackaddr$prim55453 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54188)
store volatile %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$prim55453, align 8
%stackaddr$prim55454 = alloca %struct.ScmObj*, align 8
%current_45args54189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54188)
store volatile %struct.ScmObj* %current_45args54189, %struct.ScmObj** %stackaddr$prim55454, align 8
%stackaddr$prim55455 = alloca %struct.ScmObj*, align 8
%lst47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54189)
store volatile %struct.ScmObj* %lst47139, %struct.ScmObj** %stackaddr$prim55455, align 8
%ae50329 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55456 = alloca %struct.ScmObj*, align 8
%lst47141 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50329, %struct.ScmObj* %lst47139)
store volatile %struct.ScmObj* %lst47141, %struct.ScmObj** %stackaddr$prim55456, align 8
%stackaddr$makeclosure55457 = alloca %struct.ScmObj*, align 8
%fptrToInt55458 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50331 to i64
%ae50331 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55458)
store volatile %struct.ScmObj* %ae50331, %struct.ScmObj** %stackaddr$makeclosure55457, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50331, %struct.ScmObj* %k47430, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50331, %struct.ScmObj* %lst47141, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50331, %struct.ScmObj* %v47140, i64 2)
%ae50332 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55459 = alloca %struct.ScmObj*, align 8
%fptrToInt55460 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50333 to i64
%ae50333 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55460)
store volatile %struct.ScmObj* %ae50333, %struct.ScmObj** %stackaddr$makeclosure55459, align 8
%argslist54211$ae503310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55461 = alloca %struct.ScmObj*, align 8
%argslist54211$ae503311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50333, %struct.ScmObj* %argslist54211$ae503310)
store volatile %struct.ScmObj* %argslist54211$ae503311, %struct.ScmObj** %stackaddr$prim55461, align 8
%stackaddr$prim55462 = alloca %struct.ScmObj*, align 8
%argslist54211$ae503312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50332, %struct.ScmObj* %argslist54211$ae503311)
store volatile %struct.ScmObj* %argslist54211$ae503312, %struct.ScmObj** %stackaddr$prim55462, align 8
%clofunc55463 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50331)
musttail call tailcc void %clofunc55463(%struct.ScmObj* %ae50331, %struct.ScmObj* %argslist54211$ae503312)
ret void
}

define tailcc void @proc_clo$ae50331(%struct.ScmObj* %env$ae50331,%struct.ScmObj* %current_45args54191) {
%stackaddr$env-ref55464 = alloca %struct.ScmObj*, align 8
%k47430 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50331, i64 0)
store %struct.ScmObj* %k47430, %struct.ScmObj** %stackaddr$env-ref55464
%stackaddr$env-ref55465 = alloca %struct.ScmObj*, align 8
%lst47141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50331, i64 1)
store %struct.ScmObj* %lst47141, %struct.ScmObj** %stackaddr$env-ref55465
%stackaddr$env-ref55466 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50331, i64 2)
store %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$env-ref55466
%stackaddr$prim55467 = alloca %struct.ScmObj*, align 8
%_95k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54191)
store volatile %struct.ScmObj* %_95k47431, %struct.ScmObj** %stackaddr$prim55467, align 8
%stackaddr$prim55468 = alloca %struct.ScmObj*, align 8
%current_45args54192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54191)
store volatile %struct.ScmObj* %current_45args54192, %struct.ScmObj** %stackaddr$prim55468, align 8
%stackaddr$prim55469 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54192)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim55469, align 8
%stackaddr$makeclosure55470 = alloca %struct.ScmObj*, align 8
%fptrToInt55471 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50347 to i64
%ae50347 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55471)
store volatile %struct.ScmObj* %ae50347, %struct.ScmObj** %stackaddr$makeclosure55470, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50347, %struct.ScmObj* %k47430, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50347, %struct.ScmObj* %lst47141, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50347, %struct.ScmObj* %v47140, i64 2)
%stackaddr$makeclosure55472 = alloca %struct.ScmObj*, align 8
%fptrToInt55473 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50348 to i64
%ae50348 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55473)
store volatile %struct.ScmObj* %ae50348, %struct.ScmObj** %stackaddr$makeclosure55472, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50348, %struct.ScmObj* %k47430, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50348, %struct.ScmObj* %lst47141, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50348, %struct.ScmObj* %v47140, i64 2)
%argslist54206$anf_45bind472980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55474 = alloca %struct.ScmObj*, align 8
%argslist54206$anf_45bind472981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50348, %struct.ScmObj* %argslist54206$anf_45bind472980)
store volatile %struct.ScmObj* %argslist54206$anf_45bind472981, %struct.ScmObj** %stackaddr$prim55474, align 8
%stackaddr$prim55475 = alloca %struct.ScmObj*, align 8
%argslist54206$anf_45bind472982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50347, %struct.ScmObj* %argslist54206$anf_45bind472981)
store volatile %struct.ScmObj* %argslist54206$anf_45bind472982, %struct.ScmObj** %stackaddr$prim55475, align 8
%clofunc55476 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47298)
musttail call tailcc void %clofunc55476(%struct.ScmObj* %anf_45bind47298, %struct.ScmObj* %argslist54206$anf_45bind472982)
ret void
}

define tailcc void @proc_clo$ae50347(%struct.ScmObj* %env$ae50347,%struct.ScmObj* %current_45args54194) {
%stackaddr$env-ref55477 = alloca %struct.ScmObj*, align 8
%k47430 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50347, i64 0)
store %struct.ScmObj* %k47430, %struct.ScmObj** %stackaddr$env-ref55477
%stackaddr$env-ref55478 = alloca %struct.ScmObj*, align 8
%lst47141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50347, i64 1)
store %struct.ScmObj* %lst47141, %struct.ScmObj** %stackaddr$env-ref55478
%stackaddr$env-ref55479 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50347, i64 2)
store %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$env-ref55479
%stackaddr$prim55480 = alloca %struct.ScmObj*, align 8
%_95k47432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54194)
store volatile %struct.ScmObj* %_95k47432, %struct.ScmObj** %stackaddr$prim55480, align 8
%stackaddr$prim55481 = alloca %struct.ScmObj*, align 8
%current_45args54195 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54194)
store volatile %struct.ScmObj* %current_45args54195, %struct.ScmObj** %stackaddr$prim55481, align 8
%stackaddr$prim55482 = alloca %struct.ScmObj*, align 8
%cc47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54195)
store volatile %struct.ScmObj* %cc47142, %struct.ScmObj** %stackaddr$prim55482, align 8
%ae50456 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55483 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50456)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim55483, align 8
%stackaddr$prim55484 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47299)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim55484, align 8
%truthy$cmp55485 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47300)
%cmp$cmp55485 = icmp eq i64 %truthy$cmp55485, 1
br i1 %cmp$cmp55485, label %truebranch$cmp55485, label %falsebranch$cmp55485
truebranch$cmp55485:
%ae50460 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50461 = call %struct.ScmObj* @const_init_false()
%argslist54197$k474300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55486 = alloca %struct.ScmObj*, align 8
%argslist54197$k474301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50461, %struct.ScmObj* %argslist54197$k474300)
store volatile %struct.ScmObj* %argslist54197$k474301, %struct.ScmObj** %stackaddr$prim55486, align 8
%stackaddr$prim55487 = alloca %struct.ScmObj*, align 8
%argslist54197$k474302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50460, %struct.ScmObj* %argslist54197$k474301)
store volatile %struct.ScmObj* %argslist54197$k474302, %struct.ScmObj** %stackaddr$prim55487, align 8
%clofunc55488 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47430)
musttail call tailcc void %clofunc55488(%struct.ScmObj* %k47430, %struct.ScmObj* %argslist54197$k474302)
ret void
falsebranch$cmp55485:
%ae50469 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55489 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50469)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim55489, align 8
%stackaddr$prim55490 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47301)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim55490, align 8
%stackaddr$prim55491 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47302, %struct.ScmObj* %v47140)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim55491, align 8
%truthy$cmp55492 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47303)
%cmp$cmp55492 = icmp eq i64 %truthy$cmp55492, 1
br i1 %cmp$cmp55492, label %truebranch$cmp55492, label %falsebranch$cmp55492
truebranch$cmp55492:
%ae50475 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55493 = alloca %struct.ScmObj*, align 8
%cpsprim47433 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50475)
store volatile %struct.ScmObj* %cpsprim47433, %struct.ScmObj** %stackaddr$prim55493, align 8
%ae50477 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54198$k474300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55494 = alloca %struct.ScmObj*, align 8
%argslist54198$k474301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47433, %struct.ScmObj* %argslist54198$k474300)
store volatile %struct.ScmObj* %argslist54198$k474301, %struct.ScmObj** %stackaddr$prim55494, align 8
%stackaddr$prim55495 = alloca %struct.ScmObj*, align 8
%argslist54198$k474302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50477, %struct.ScmObj* %argslist54198$k474301)
store volatile %struct.ScmObj* %argslist54198$k474302, %struct.ScmObj** %stackaddr$prim55495, align 8
%clofunc55496 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47430)
musttail call tailcc void %clofunc55496(%struct.ScmObj* %k47430, %struct.ScmObj* %argslist54198$k474302)
ret void
falsebranch$cmp55492:
%ae50488 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55497 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50488)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim55497, align 8
%stackaddr$prim55498 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47304)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim55498, align 8
%ae50491 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%_95047144 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50491, %struct.ScmObj* %anf_45bind47305)
store volatile %struct.ScmObj* %_95047144, %struct.ScmObj** %stackaddr$prim55499, align 8
%argslist54199$cc471420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55500 = alloca %struct.ScmObj*, align 8
%argslist54199$cc471421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47142, %struct.ScmObj* %argslist54199$cc471420)
store volatile %struct.ScmObj* %argslist54199$cc471421, %struct.ScmObj** %stackaddr$prim55500, align 8
%stackaddr$prim55501 = alloca %struct.ScmObj*, align 8
%argslist54199$cc471422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47430, %struct.ScmObj* %argslist54199$cc471421)
store volatile %struct.ScmObj* %argslist54199$cc471422, %struct.ScmObj** %stackaddr$prim55501, align 8
%clofunc55502 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47142)
musttail call tailcc void %clofunc55502(%struct.ScmObj* %cc47142, %struct.ScmObj* %argslist54199$cc471422)
ret void
}

define tailcc void @proc_clo$ae50348(%struct.ScmObj* %env$ae50348,%struct.ScmObj* %current_45args54200) {
%stackaddr$env-ref55503 = alloca %struct.ScmObj*, align 8
%k47430 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50348, i64 0)
store %struct.ScmObj* %k47430, %struct.ScmObj** %stackaddr$env-ref55503
%stackaddr$env-ref55504 = alloca %struct.ScmObj*, align 8
%lst47141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50348, i64 1)
store %struct.ScmObj* %lst47141, %struct.ScmObj** %stackaddr$env-ref55504
%stackaddr$env-ref55505 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50348, i64 2)
store %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$env-ref55505
%stackaddr$prim55506 = alloca %struct.ScmObj*, align 8
%_95k47432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54200)
store volatile %struct.ScmObj* %_95k47432, %struct.ScmObj** %stackaddr$prim55506, align 8
%stackaddr$prim55507 = alloca %struct.ScmObj*, align 8
%current_45args54201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54200)
store volatile %struct.ScmObj* %current_45args54201, %struct.ScmObj** %stackaddr$prim55507, align 8
%stackaddr$prim55508 = alloca %struct.ScmObj*, align 8
%cc47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54201)
store volatile %struct.ScmObj* %cc47142, %struct.ScmObj** %stackaddr$prim55508, align 8
%ae50350 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55509 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50350)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim55509, align 8
%stackaddr$prim55510 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47299)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim55510, align 8
%truthy$cmp55511 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47300)
%cmp$cmp55511 = icmp eq i64 %truthy$cmp55511, 1
br i1 %cmp$cmp55511, label %truebranch$cmp55511, label %falsebranch$cmp55511
truebranch$cmp55511:
%ae50354 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50355 = call %struct.ScmObj* @const_init_false()
%argslist54203$k474300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55512 = alloca %struct.ScmObj*, align 8
%argslist54203$k474301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50355, %struct.ScmObj* %argslist54203$k474300)
store volatile %struct.ScmObj* %argslist54203$k474301, %struct.ScmObj** %stackaddr$prim55512, align 8
%stackaddr$prim55513 = alloca %struct.ScmObj*, align 8
%argslist54203$k474302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50354, %struct.ScmObj* %argslist54203$k474301)
store volatile %struct.ScmObj* %argslist54203$k474302, %struct.ScmObj** %stackaddr$prim55513, align 8
%clofunc55514 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47430)
musttail call tailcc void %clofunc55514(%struct.ScmObj* %k47430, %struct.ScmObj* %argslist54203$k474302)
ret void
falsebranch$cmp55511:
%ae50363 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55515 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50363)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim55515, align 8
%stackaddr$prim55516 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47301)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim55516, align 8
%stackaddr$prim55517 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47302, %struct.ScmObj* %v47140)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim55517, align 8
%truthy$cmp55518 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47303)
%cmp$cmp55518 = icmp eq i64 %truthy$cmp55518, 1
br i1 %cmp$cmp55518, label %truebranch$cmp55518, label %falsebranch$cmp55518
truebranch$cmp55518:
%ae50369 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55519 = alloca %struct.ScmObj*, align 8
%cpsprim47433 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50369)
store volatile %struct.ScmObj* %cpsprim47433, %struct.ScmObj** %stackaddr$prim55519, align 8
%ae50371 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54204$k474300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55520 = alloca %struct.ScmObj*, align 8
%argslist54204$k474301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47433, %struct.ScmObj* %argslist54204$k474300)
store volatile %struct.ScmObj* %argslist54204$k474301, %struct.ScmObj** %stackaddr$prim55520, align 8
%stackaddr$prim55521 = alloca %struct.ScmObj*, align 8
%argslist54204$k474302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50371, %struct.ScmObj* %argslist54204$k474301)
store volatile %struct.ScmObj* %argslist54204$k474302, %struct.ScmObj** %stackaddr$prim55521, align 8
%clofunc55522 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47430)
musttail call tailcc void %clofunc55522(%struct.ScmObj* %k47430, %struct.ScmObj* %argslist54204$k474302)
ret void
falsebranch$cmp55518:
%ae50382 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55523 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50382)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim55523, align 8
%stackaddr$prim55524 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47304)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim55524, align 8
%ae50385 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55525 = alloca %struct.ScmObj*, align 8
%_95047144 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50385, %struct.ScmObj* %anf_45bind47305)
store volatile %struct.ScmObj* %_95047144, %struct.ScmObj** %stackaddr$prim55525, align 8
%argslist54205$cc471420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55526 = alloca %struct.ScmObj*, align 8
%argslist54205$cc471421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47142, %struct.ScmObj* %argslist54205$cc471420)
store volatile %struct.ScmObj* %argslist54205$cc471421, %struct.ScmObj** %stackaddr$prim55526, align 8
%stackaddr$prim55527 = alloca %struct.ScmObj*, align 8
%argslist54205$cc471422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47430, %struct.ScmObj* %argslist54205$cc471421)
store volatile %struct.ScmObj* %argslist54205$cc471422, %struct.ScmObj** %stackaddr$prim55527, align 8
%clofunc55528 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47142)
musttail call tailcc void %clofunc55528(%struct.ScmObj* %cc47142, %struct.ScmObj* %argslist54205$cc471422)
ret void
}

define tailcc void @proc_clo$ae50333(%struct.ScmObj* %env$ae50333,%struct.ScmObj* %current_45args54207) {
%stackaddr$prim55529 = alloca %struct.ScmObj*, align 8
%k47434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54207)
store volatile %struct.ScmObj* %k47434, %struct.ScmObj** %stackaddr$prim55529, align 8
%stackaddr$prim55530 = alloca %struct.ScmObj*, align 8
%current_45args54208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54207)
store volatile %struct.ScmObj* %current_45args54208, %struct.ScmObj** %stackaddr$prim55530, align 8
%stackaddr$prim55531 = alloca %struct.ScmObj*, align 8
%u47143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54208)
store volatile %struct.ScmObj* %u47143, %struct.ScmObj** %stackaddr$prim55531, align 8
%argslist54210$u471430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55532 = alloca %struct.ScmObj*, align 8
%argslist54210$u471431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47143, %struct.ScmObj* %argslist54210$u471430)
store volatile %struct.ScmObj* %argslist54210$u471431, %struct.ScmObj** %stackaddr$prim55532, align 8
%stackaddr$prim55533 = alloca %struct.ScmObj*, align 8
%argslist54210$u471432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47434, %struct.ScmObj* %argslist54210$u471431)
store volatile %struct.ScmObj* %argslist54210$u471432, %struct.ScmObj** %stackaddr$prim55533, align 8
%clofunc55534 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47143)
musttail call tailcc void %clofunc55534(%struct.ScmObj* %u47143, %struct.ScmObj* %argslist54210$u471432)
ret void
}

define tailcc void @proc_clo$ae49792(%struct.ScmObj* %env$ae49792,%struct.ScmObj* %current_45args54213) {
%stackaddr$prim55535 = alloca %struct.ScmObj*, align 8
%k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54213)
store volatile %struct.ScmObj* %k47435, %struct.ScmObj** %stackaddr$prim55535, align 8
%stackaddr$prim55536 = alloca %struct.ScmObj*, align 8
%current_45args54214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54213)
store volatile %struct.ScmObj* %current_45args54214, %struct.ScmObj** %stackaddr$prim55536, align 8
%stackaddr$prim55537 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54214)
store volatile %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$prim55537, align 8
%stackaddr$prim55538 = alloca %struct.ScmObj*, align 8
%current_45args54215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54214)
store volatile %struct.ScmObj* %current_45args54215, %struct.ScmObj** %stackaddr$prim55538, align 8
%stackaddr$prim55539 = alloca %struct.ScmObj*, align 8
%n47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54215)
store volatile %struct.ScmObj* %n47146, %struct.ScmObj** %stackaddr$prim55539, align 8
%ae49793 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55540 = alloca %struct.ScmObj*, align 8
%n47149 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49793, %struct.ScmObj* %n47146)
store volatile %struct.ScmObj* %n47149, %struct.ScmObj** %stackaddr$prim55540, align 8
%ae49795 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55541 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49795, %struct.ScmObj* %lst47147)
store volatile %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$prim55541, align 8
%stackaddr$makeclosure55542 = alloca %struct.ScmObj*, align 8
%fptrToInt55543 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49797 to i64
%ae49797 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55543)
store volatile %struct.ScmObj* %ae49797, %struct.ScmObj** %stackaddr$makeclosure55542, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %n47149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %lst47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %k47435, i64 2)
%ae49798 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55544 = alloca %struct.ScmObj*, align 8
%fptrToInt55545 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49799 to i64
%ae49799 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55545)
store volatile %struct.ScmObj* %ae49799, %struct.ScmObj** %stackaddr$makeclosure55544, align 8
%argslist54235$ae497970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55546 = alloca %struct.ScmObj*, align 8
%argslist54235$ae497971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49799, %struct.ScmObj* %argslist54235$ae497970)
store volatile %struct.ScmObj* %argslist54235$ae497971, %struct.ScmObj** %stackaddr$prim55546, align 8
%stackaddr$prim55547 = alloca %struct.ScmObj*, align 8
%argslist54235$ae497972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49798, %struct.ScmObj* %argslist54235$ae497971)
store volatile %struct.ScmObj* %argslist54235$ae497972, %struct.ScmObj** %stackaddr$prim55547, align 8
%clofunc55548 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49797)
musttail call tailcc void %clofunc55548(%struct.ScmObj* %ae49797, %struct.ScmObj* %argslist54235$ae497972)
ret void
}

define tailcc void @proc_clo$ae49797(%struct.ScmObj* %env$ae49797,%struct.ScmObj* %current_45args54217) {
%stackaddr$env-ref55549 = alloca %struct.ScmObj*, align 8
%n47149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 0)
store %struct.ScmObj* %n47149, %struct.ScmObj** %stackaddr$env-ref55549
%stackaddr$env-ref55550 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 1)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref55550
%stackaddr$env-ref55551 = alloca %struct.ScmObj*, align 8
%k47435 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 2)
store %struct.ScmObj* %k47435, %struct.ScmObj** %stackaddr$env-ref55551
%stackaddr$prim55552 = alloca %struct.ScmObj*, align 8
%_95k47436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54217)
store volatile %struct.ScmObj* %_95k47436, %struct.ScmObj** %stackaddr$prim55552, align 8
%stackaddr$prim55553 = alloca %struct.ScmObj*, align 8
%current_45args54218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54217)
store volatile %struct.ScmObj* %current_45args54218, %struct.ScmObj** %stackaddr$prim55553, align 8
%stackaddr$prim55554 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54218)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim55554, align 8
%stackaddr$makeclosure55555 = alloca %struct.ScmObj*, align 8
%fptrToInt55556 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49813 to i64
%ae49813 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55556)
store volatile %struct.ScmObj* %ae49813, %struct.ScmObj** %stackaddr$makeclosure55555, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49813, %struct.ScmObj* %n47149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49813, %struct.ScmObj* %lst47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49813, %struct.ScmObj* %k47435, i64 2)
%stackaddr$makeclosure55557 = alloca %struct.ScmObj*, align 8
%fptrToInt55558 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49814 to i64
%ae49814 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55558)
store volatile %struct.ScmObj* %ae49814, %struct.ScmObj** %stackaddr$makeclosure55557, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49814, %struct.ScmObj* %n47149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49814, %struct.ScmObj* %lst47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49814, %struct.ScmObj* %k47435, i64 2)
%argslist54230$anf_45bind472910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55559 = alloca %struct.ScmObj*, align 8
%argslist54230$anf_45bind472911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49814, %struct.ScmObj* %argslist54230$anf_45bind472910)
store volatile %struct.ScmObj* %argslist54230$anf_45bind472911, %struct.ScmObj** %stackaddr$prim55559, align 8
%stackaddr$prim55560 = alloca %struct.ScmObj*, align 8
%argslist54230$anf_45bind472912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49813, %struct.ScmObj* %argslist54230$anf_45bind472911)
store volatile %struct.ScmObj* %argslist54230$anf_45bind472912, %struct.ScmObj** %stackaddr$prim55560, align 8
%clofunc55561 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47291)
musttail call tailcc void %clofunc55561(%struct.ScmObj* %anf_45bind47291, %struct.ScmObj* %argslist54230$anf_45bind472912)
ret void
}

define tailcc void @proc_clo$ae49813(%struct.ScmObj* %env$ae49813,%struct.ScmObj* %current_45args54220) {
%stackaddr$env-ref55562 = alloca %struct.ScmObj*, align 8
%n47149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49813, i64 0)
store %struct.ScmObj* %n47149, %struct.ScmObj** %stackaddr$env-ref55562
%stackaddr$env-ref55563 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49813, i64 1)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref55563
%stackaddr$env-ref55564 = alloca %struct.ScmObj*, align 8
%k47435 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49813, i64 2)
store %struct.ScmObj* %k47435, %struct.ScmObj** %stackaddr$env-ref55564
%stackaddr$prim55565 = alloca %struct.ScmObj*, align 8
%_95k47437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54220)
store volatile %struct.ScmObj* %_95k47437, %struct.ScmObj** %stackaddr$prim55565, align 8
%stackaddr$prim55566 = alloca %struct.ScmObj*, align 8
%current_45args54221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54220)
store volatile %struct.ScmObj* %current_45args54221, %struct.ScmObj** %stackaddr$prim55566, align 8
%stackaddr$prim55567 = alloca %struct.ScmObj*, align 8
%cc47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54221)
store volatile %struct.ScmObj* %cc47150, %struct.ScmObj** %stackaddr$prim55567, align 8
%ae49956 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55568 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49956)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim55568, align 8
%ae49957 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55569 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49957, %struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim55569, align 8
%truthy$cmp55570 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47293)
%cmp$cmp55570 = icmp eq i64 %truthy$cmp55570, 1
br i1 %cmp$cmp55570, label %truebranch$cmp55570, label %falsebranch$cmp55570
truebranch$cmp55570:
%ae49961 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55571 = alloca %struct.ScmObj*, align 8
%cpsprim47438 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49961)
store volatile %struct.ScmObj* %cpsprim47438, %struct.ScmObj** %stackaddr$prim55571, align 8
%ae49963 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54223$k474350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55572 = alloca %struct.ScmObj*, align 8
%argslist54223$k474351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47438, %struct.ScmObj* %argslist54223$k474350)
store volatile %struct.ScmObj* %argslist54223$k474351, %struct.ScmObj** %stackaddr$prim55572, align 8
%stackaddr$prim55573 = alloca %struct.ScmObj*, align 8
%argslist54223$k474352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49963, %struct.ScmObj* %argslist54223$k474351)
store volatile %struct.ScmObj* %argslist54223$k474352, %struct.ScmObj** %stackaddr$prim55573, align 8
%clofunc55574 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47435)
musttail call tailcc void %clofunc55574(%struct.ScmObj* %k47435, %struct.ScmObj* %argslist54223$k474352)
ret void
falsebranch$cmp55570:
%ae49974 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55575 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49974)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim55575, align 8
%stackaddr$prim55576 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim55576, align 8
%ae49977 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55577 = alloca %struct.ScmObj*, align 8
%_95047153 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49977, %struct.ScmObj* %anf_45bind47295)
store volatile %struct.ScmObj* %_95047153, %struct.ScmObj** %stackaddr$prim55577, align 8
%ae49980 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55578 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49980)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim55578, align 8
%ae49982 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55579 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47296, %struct.ScmObj* %ae49982)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim55579, align 8
%ae49984 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55580 = alloca %struct.ScmObj*, align 8
%_95147152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49984, %struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %_95147152, %struct.ScmObj** %stackaddr$prim55580, align 8
%argslist54224$cc471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55581 = alloca %struct.ScmObj*, align 8
%argslist54224$cc471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47150, %struct.ScmObj* %argslist54224$cc471500)
store volatile %struct.ScmObj* %argslist54224$cc471501, %struct.ScmObj** %stackaddr$prim55581, align 8
%stackaddr$prim55582 = alloca %struct.ScmObj*, align 8
%argslist54224$cc471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47435, %struct.ScmObj* %argslist54224$cc471501)
store volatile %struct.ScmObj* %argslist54224$cc471502, %struct.ScmObj** %stackaddr$prim55582, align 8
%clofunc55583 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47150)
musttail call tailcc void %clofunc55583(%struct.ScmObj* %cc47150, %struct.ScmObj* %argslist54224$cc471502)
ret void
}

define tailcc void @proc_clo$ae49814(%struct.ScmObj* %env$ae49814,%struct.ScmObj* %current_45args54225) {
%stackaddr$env-ref55584 = alloca %struct.ScmObj*, align 8
%n47149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49814, i64 0)
store %struct.ScmObj* %n47149, %struct.ScmObj** %stackaddr$env-ref55584
%stackaddr$env-ref55585 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49814, i64 1)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref55585
%stackaddr$env-ref55586 = alloca %struct.ScmObj*, align 8
%k47435 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49814, i64 2)
store %struct.ScmObj* %k47435, %struct.ScmObj** %stackaddr$env-ref55586
%stackaddr$prim55587 = alloca %struct.ScmObj*, align 8
%_95k47437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54225)
store volatile %struct.ScmObj* %_95k47437, %struct.ScmObj** %stackaddr$prim55587, align 8
%stackaddr$prim55588 = alloca %struct.ScmObj*, align 8
%current_45args54226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54225)
store volatile %struct.ScmObj* %current_45args54226, %struct.ScmObj** %stackaddr$prim55588, align 8
%stackaddr$prim55589 = alloca %struct.ScmObj*, align 8
%cc47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54226)
store volatile %struct.ScmObj* %cc47150, %struct.ScmObj** %stackaddr$prim55589, align 8
%ae49816 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55590 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49816)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim55590, align 8
%ae49817 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55591 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49817, %struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim55591, align 8
%truthy$cmp55592 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47293)
%cmp$cmp55592 = icmp eq i64 %truthy$cmp55592, 1
br i1 %cmp$cmp55592, label %truebranch$cmp55592, label %falsebranch$cmp55592
truebranch$cmp55592:
%ae49821 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55593 = alloca %struct.ScmObj*, align 8
%cpsprim47438 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49821)
store volatile %struct.ScmObj* %cpsprim47438, %struct.ScmObj** %stackaddr$prim55593, align 8
%ae49823 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54228$k474350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55594 = alloca %struct.ScmObj*, align 8
%argslist54228$k474351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47438, %struct.ScmObj* %argslist54228$k474350)
store volatile %struct.ScmObj* %argslist54228$k474351, %struct.ScmObj** %stackaddr$prim55594, align 8
%stackaddr$prim55595 = alloca %struct.ScmObj*, align 8
%argslist54228$k474352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49823, %struct.ScmObj* %argslist54228$k474351)
store volatile %struct.ScmObj* %argslist54228$k474352, %struct.ScmObj** %stackaddr$prim55595, align 8
%clofunc55596 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47435)
musttail call tailcc void %clofunc55596(%struct.ScmObj* %k47435, %struct.ScmObj* %argslist54228$k474352)
ret void
falsebranch$cmp55592:
%ae49834 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55597 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49834)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim55597, align 8
%stackaddr$prim55598 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim55598, align 8
%ae49837 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55599 = alloca %struct.ScmObj*, align 8
%_95047153 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49837, %struct.ScmObj* %anf_45bind47295)
store volatile %struct.ScmObj* %_95047153, %struct.ScmObj** %stackaddr$prim55599, align 8
%ae49840 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55600 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49840)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim55600, align 8
%ae49842 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55601 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47296, %struct.ScmObj* %ae49842)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim55601, align 8
%ae49844 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55602 = alloca %struct.ScmObj*, align 8
%_95147152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49844, %struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %_95147152, %struct.ScmObj** %stackaddr$prim55602, align 8
%argslist54229$cc471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55603 = alloca %struct.ScmObj*, align 8
%argslist54229$cc471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47150, %struct.ScmObj* %argslist54229$cc471500)
store volatile %struct.ScmObj* %argslist54229$cc471501, %struct.ScmObj** %stackaddr$prim55603, align 8
%stackaddr$prim55604 = alloca %struct.ScmObj*, align 8
%argslist54229$cc471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47435, %struct.ScmObj* %argslist54229$cc471501)
store volatile %struct.ScmObj* %argslist54229$cc471502, %struct.ScmObj** %stackaddr$prim55604, align 8
%clofunc55605 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47150)
musttail call tailcc void %clofunc55605(%struct.ScmObj* %cc47150, %struct.ScmObj* %argslist54229$cc471502)
ret void
}

define tailcc void @proc_clo$ae49799(%struct.ScmObj* %env$ae49799,%struct.ScmObj* %current_45args54231) {
%stackaddr$prim55606 = alloca %struct.ScmObj*, align 8
%k47439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54231)
store volatile %struct.ScmObj* %k47439, %struct.ScmObj** %stackaddr$prim55606, align 8
%stackaddr$prim55607 = alloca %struct.ScmObj*, align 8
%current_45args54232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54231)
store volatile %struct.ScmObj* %current_45args54232, %struct.ScmObj** %stackaddr$prim55607, align 8
%stackaddr$prim55608 = alloca %struct.ScmObj*, align 8
%u47151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54232)
store volatile %struct.ScmObj* %u47151, %struct.ScmObj** %stackaddr$prim55608, align 8
%argslist54234$u471510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55609 = alloca %struct.ScmObj*, align 8
%argslist54234$u471511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47151, %struct.ScmObj* %argslist54234$u471510)
store volatile %struct.ScmObj* %argslist54234$u471511, %struct.ScmObj** %stackaddr$prim55609, align 8
%stackaddr$prim55610 = alloca %struct.ScmObj*, align 8
%argslist54234$u471512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47439, %struct.ScmObj* %argslist54234$u471511)
store volatile %struct.ScmObj* %argslist54234$u471512, %struct.ScmObj** %stackaddr$prim55610, align 8
%clofunc55611 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47151)
musttail call tailcc void %clofunc55611(%struct.ScmObj* %u47151, %struct.ScmObj* %argslist54234$u471512)
ret void
}

define tailcc void @proc_clo$ae49376(%struct.ScmObj* %env$ae49376,%struct.ScmObj* %current_45args54237) {
%stackaddr$prim55612 = alloca %struct.ScmObj*, align 8
%k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54237)
store volatile %struct.ScmObj* %k47440, %struct.ScmObj** %stackaddr$prim55612, align 8
%stackaddr$prim55613 = alloca %struct.ScmObj*, align 8
%current_45args54238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54237)
store volatile %struct.ScmObj* %current_45args54238, %struct.ScmObj** %stackaddr$prim55613, align 8
%stackaddr$prim55614 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54238)
store volatile %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$prim55614, align 8
%ae49377 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55615 = alloca %struct.ScmObj*, align 8
%a47156 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49377, %struct.ScmObj* %a47155)
store volatile %struct.ScmObj* %a47156, %struct.ScmObj** %stackaddr$prim55615, align 8
%stackaddr$makeclosure55616 = alloca %struct.ScmObj*, align 8
%fptrToInt55617 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49379 to i64
%ae49379 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55617)
store volatile %struct.ScmObj* %ae49379, %struct.ScmObj** %stackaddr$makeclosure55616, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49379, %struct.ScmObj* %a47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49379, %struct.ScmObj* %k47440, i64 1)
%ae49380 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55618 = alloca %struct.ScmObj*, align 8
%fptrToInt55619 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49381 to i64
%ae49381 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55619)
store volatile %struct.ScmObj* %ae49381, %struct.ScmObj** %stackaddr$makeclosure55618, align 8
%argslist54260$ae493790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55620 = alloca %struct.ScmObj*, align 8
%argslist54260$ae493791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49381, %struct.ScmObj* %argslist54260$ae493790)
store volatile %struct.ScmObj* %argslist54260$ae493791, %struct.ScmObj** %stackaddr$prim55620, align 8
%stackaddr$prim55621 = alloca %struct.ScmObj*, align 8
%argslist54260$ae493792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49380, %struct.ScmObj* %argslist54260$ae493791)
store volatile %struct.ScmObj* %argslist54260$ae493792, %struct.ScmObj** %stackaddr$prim55621, align 8
%clofunc55622 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49379)
musttail call tailcc void %clofunc55622(%struct.ScmObj* %ae49379, %struct.ScmObj* %argslist54260$ae493792)
ret void
}

define tailcc void @proc_clo$ae49379(%struct.ScmObj* %env$ae49379,%struct.ScmObj* %current_45args54240) {
%stackaddr$env-ref55623 = alloca %struct.ScmObj*, align 8
%a47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49379, i64 0)
store %struct.ScmObj* %a47156, %struct.ScmObj** %stackaddr$env-ref55623
%stackaddr$env-ref55624 = alloca %struct.ScmObj*, align 8
%k47440 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49379, i64 1)
store %struct.ScmObj* %k47440, %struct.ScmObj** %stackaddr$env-ref55624
%stackaddr$prim55625 = alloca %struct.ScmObj*, align 8
%_95k47441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54240)
store volatile %struct.ScmObj* %_95k47441, %struct.ScmObj** %stackaddr$prim55625, align 8
%stackaddr$prim55626 = alloca %struct.ScmObj*, align 8
%current_45args54241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54240)
store volatile %struct.ScmObj* %current_45args54241, %struct.ScmObj** %stackaddr$prim55626, align 8
%stackaddr$prim55627 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54241)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim55627, align 8
%stackaddr$makeclosure55628 = alloca %struct.ScmObj*, align 8
%fptrToInt55629 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49398 to i64
%ae49398 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55629)
store volatile %struct.ScmObj* %ae49398, %struct.ScmObj** %stackaddr$makeclosure55628, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49398, %struct.ScmObj* %a47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49398, %struct.ScmObj* %k47440, i64 1)
%stackaddr$makeclosure55630 = alloca %struct.ScmObj*, align 8
%fptrToInt55631 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49399 to i64
%ae49399 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55631)
store volatile %struct.ScmObj* %ae49399, %struct.ScmObj** %stackaddr$makeclosure55630, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49399, %struct.ScmObj* %a47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49399, %struct.ScmObj* %k47440, i64 1)
%argslist54255$anf_45bind472830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55632 = alloca %struct.ScmObj*, align 8
%argslist54255$anf_45bind472831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49399, %struct.ScmObj* %argslist54255$anf_45bind472830)
store volatile %struct.ScmObj* %argslist54255$anf_45bind472831, %struct.ScmObj** %stackaddr$prim55632, align 8
%stackaddr$prim55633 = alloca %struct.ScmObj*, align 8
%argslist54255$anf_45bind472832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49398, %struct.ScmObj* %argslist54255$anf_45bind472831)
store volatile %struct.ScmObj* %argslist54255$anf_45bind472832, %struct.ScmObj** %stackaddr$prim55633, align 8
%clofunc55634 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47283)
musttail call tailcc void %clofunc55634(%struct.ScmObj* %anf_45bind47283, %struct.ScmObj* %argslist54255$anf_45bind472832)
ret void
}

define tailcc void @proc_clo$ae49398(%struct.ScmObj* %env$ae49398,%struct.ScmObj* %current_45args54243) {
%stackaddr$env-ref55635 = alloca %struct.ScmObj*, align 8
%a47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49398, i64 0)
store %struct.ScmObj* %a47156, %struct.ScmObj** %stackaddr$env-ref55635
%stackaddr$env-ref55636 = alloca %struct.ScmObj*, align 8
%k47440 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49398, i64 1)
store %struct.ScmObj* %k47440, %struct.ScmObj** %stackaddr$env-ref55636
%stackaddr$prim55637 = alloca %struct.ScmObj*, align 8
%_95k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54243)
store volatile %struct.ScmObj* %_95k47442, %struct.ScmObj** %stackaddr$prim55637, align 8
%stackaddr$prim55638 = alloca %struct.ScmObj*, align 8
%current_45args54244 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54243)
store volatile %struct.ScmObj* %current_45args54244, %struct.ScmObj** %stackaddr$prim55638, align 8
%stackaddr$prim55639 = alloca %struct.ScmObj*, align 8
%cc47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54244)
store volatile %struct.ScmObj* %cc47157, %struct.ScmObj** %stackaddr$prim55639, align 8
%ae49514 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55640 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49514)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim55640, align 8
%stackaddr$prim55641 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim55641, align 8
%truthy$cmp55642 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47285)
%cmp$cmp55642 = icmp eq i64 %truthy$cmp55642, 1
br i1 %cmp$cmp55642, label %truebranch$cmp55642, label %falsebranch$cmp55642
truebranch$cmp55642:
%ae49518 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49519 = call %struct.ScmObj* @const_init_true()
%argslist54246$k474400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55643 = alloca %struct.ScmObj*, align 8
%argslist54246$k474401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49519, %struct.ScmObj* %argslist54246$k474400)
store volatile %struct.ScmObj* %argslist54246$k474401, %struct.ScmObj** %stackaddr$prim55643, align 8
%stackaddr$prim55644 = alloca %struct.ScmObj*, align 8
%argslist54246$k474402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49518, %struct.ScmObj* %argslist54246$k474401)
store volatile %struct.ScmObj* %argslist54246$k474402, %struct.ScmObj** %stackaddr$prim55644, align 8
%clofunc55645 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47440)
musttail call tailcc void %clofunc55645(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist54246$k474402)
ret void
falsebranch$cmp55642:
%ae49527 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55646 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49527)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim55646, align 8
%stackaddr$prim55647 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47286)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim55647, align 8
%truthy$cmp55648 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47287)
%cmp$cmp55648 = icmp eq i64 %truthy$cmp55648, 1
br i1 %cmp$cmp55648, label %truebranch$cmp55648, label %falsebranch$cmp55648
truebranch$cmp55648:
%ae49531 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55649 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49531)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim55649, align 8
%stackaddr$prim55650 = alloca %struct.ScmObj*, align 8
%b47159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %b47159, %struct.ScmObj** %stackaddr$prim55650, align 8
%ae49534 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55651 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49534)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim55651, align 8
%stackaddr$prim55652 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim55652, align 8
%ae49537 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55653 = alloca %struct.ScmObj*, align 8
%_95047160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49537, %struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %_95047160, %struct.ScmObj** %stackaddr$prim55653, align 8
%argslist54247$cc471570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55654 = alloca %struct.ScmObj*, align 8
%argslist54247$cc471571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist54247$cc471570)
store volatile %struct.ScmObj* %argslist54247$cc471571, %struct.ScmObj** %stackaddr$prim55654, align 8
%stackaddr$prim55655 = alloca %struct.ScmObj*, align 8
%argslist54247$cc471572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist54247$cc471571)
store volatile %struct.ScmObj* %argslist54247$cc471572, %struct.ScmObj** %stackaddr$prim55655, align 8
%clofunc55656 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47157)
musttail call tailcc void %clofunc55656(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist54247$cc471572)
ret void
falsebranch$cmp55648:
%ae49570 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49571 = call %struct.ScmObj* @const_init_false()
%argslist54248$k474400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55657 = alloca %struct.ScmObj*, align 8
%argslist54248$k474401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49571, %struct.ScmObj* %argslist54248$k474400)
store volatile %struct.ScmObj* %argslist54248$k474401, %struct.ScmObj** %stackaddr$prim55657, align 8
%stackaddr$prim55658 = alloca %struct.ScmObj*, align 8
%argslist54248$k474402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49570, %struct.ScmObj* %argslist54248$k474401)
store volatile %struct.ScmObj* %argslist54248$k474402, %struct.ScmObj** %stackaddr$prim55658, align 8
%clofunc55659 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47440)
musttail call tailcc void %clofunc55659(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist54248$k474402)
ret void
}

define tailcc void @proc_clo$ae49399(%struct.ScmObj* %env$ae49399,%struct.ScmObj* %current_45args54249) {
%stackaddr$env-ref55660 = alloca %struct.ScmObj*, align 8
%a47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49399, i64 0)
store %struct.ScmObj* %a47156, %struct.ScmObj** %stackaddr$env-ref55660
%stackaddr$env-ref55661 = alloca %struct.ScmObj*, align 8
%k47440 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49399, i64 1)
store %struct.ScmObj* %k47440, %struct.ScmObj** %stackaddr$env-ref55661
%stackaddr$prim55662 = alloca %struct.ScmObj*, align 8
%_95k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54249)
store volatile %struct.ScmObj* %_95k47442, %struct.ScmObj** %stackaddr$prim55662, align 8
%stackaddr$prim55663 = alloca %struct.ScmObj*, align 8
%current_45args54250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54249)
store volatile %struct.ScmObj* %current_45args54250, %struct.ScmObj** %stackaddr$prim55663, align 8
%stackaddr$prim55664 = alloca %struct.ScmObj*, align 8
%cc47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54250)
store volatile %struct.ScmObj* %cc47157, %struct.ScmObj** %stackaddr$prim55664, align 8
%ae49401 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55665 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49401)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim55665, align 8
%stackaddr$prim55666 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim55666, align 8
%truthy$cmp55667 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47285)
%cmp$cmp55667 = icmp eq i64 %truthy$cmp55667, 1
br i1 %cmp$cmp55667, label %truebranch$cmp55667, label %falsebranch$cmp55667
truebranch$cmp55667:
%ae49405 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49406 = call %struct.ScmObj* @const_init_true()
%argslist54252$k474400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55668 = alloca %struct.ScmObj*, align 8
%argslist54252$k474401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49406, %struct.ScmObj* %argslist54252$k474400)
store volatile %struct.ScmObj* %argslist54252$k474401, %struct.ScmObj** %stackaddr$prim55668, align 8
%stackaddr$prim55669 = alloca %struct.ScmObj*, align 8
%argslist54252$k474402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49405, %struct.ScmObj* %argslist54252$k474401)
store volatile %struct.ScmObj* %argslist54252$k474402, %struct.ScmObj** %stackaddr$prim55669, align 8
%clofunc55670 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47440)
musttail call tailcc void %clofunc55670(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist54252$k474402)
ret void
falsebranch$cmp55667:
%ae49414 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55671 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49414)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim55671, align 8
%stackaddr$prim55672 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47286)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim55672, align 8
%truthy$cmp55673 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47287)
%cmp$cmp55673 = icmp eq i64 %truthy$cmp55673, 1
br i1 %cmp$cmp55673, label %truebranch$cmp55673, label %falsebranch$cmp55673
truebranch$cmp55673:
%ae49418 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55674 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49418)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim55674, align 8
%stackaddr$prim55675 = alloca %struct.ScmObj*, align 8
%b47159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %b47159, %struct.ScmObj** %stackaddr$prim55675, align 8
%ae49421 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55676 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49421)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim55676, align 8
%stackaddr$prim55677 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim55677, align 8
%ae49424 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55678 = alloca %struct.ScmObj*, align 8
%_95047160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49424, %struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %_95047160, %struct.ScmObj** %stackaddr$prim55678, align 8
%argslist54253$cc471570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55679 = alloca %struct.ScmObj*, align 8
%argslist54253$cc471571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist54253$cc471570)
store volatile %struct.ScmObj* %argslist54253$cc471571, %struct.ScmObj** %stackaddr$prim55679, align 8
%stackaddr$prim55680 = alloca %struct.ScmObj*, align 8
%argslist54253$cc471572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist54253$cc471571)
store volatile %struct.ScmObj* %argslist54253$cc471572, %struct.ScmObj** %stackaddr$prim55680, align 8
%clofunc55681 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47157)
musttail call tailcc void %clofunc55681(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist54253$cc471572)
ret void
falsebranch$cmp55673:
%ae49457 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49458 = call %struct.ScmObj* @const_init_false()
%argslist54254$k474400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55682 = alloca %struct.ScmObj*, align 8
%argslist54254$k474401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49458, %struct.ScmObj* %argslist54254$k474400)
store volatile %struct.ScmObj* %argslist54254$k474401, %struct.ScmObj** %stackaddr$prim55682, align 8
%stackaddr$prim55683 = alloca %struct.ScmObj*, align 8
%argslist54254$k474402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49457, %struct.ScmObj* %argslist54254$k474401)
store volatile %struct.ScmObj* %argslist54254$k474402, %struct.ScmObj** %stackaddr$prim55683, align 8
%clofunc55684 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47440)
musttail call tailcc void %clofunc55684(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist54254$k474402)
ret void
}

define tailcc void @proc_clo$ae49381(%struct.ScmObj* %env$ae49381,%struct.ScmObj* %current_45args54256) {
%stackaddr$prim55685 = alloca %struct.ScmObj*, align 8
%k47443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54256)
store volatile %struct.ScmObj* %k47443, %struct.ScmObj** %stackaddr$prim55685, align 8
%stackaddr$prim55686 = alloca %struct.ScmObj*, align 8
%current_45args54257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54256)
store volatile %struct.ScmObj* %current_45args54257, %struct.ScmObj** %stackaddr$prim55686, align 8
%stackaddr$prim55687 = alloca %struct.ScmObj*, align 8
%k47158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54257)
store volatile %struct.ScmObj* %k47158, %struct.ScmObj** %stackaddr$prim55687, align 8
%ae49383 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54259$k474430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55688 = alloca %struct.ScmObj*, align 8
%argslist54259$k474431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47158, %struct.ScmObj* %argslist54259$k474430)
store volatile %struct.ScmObj* %argslist54259$k474431, %struct.ScmObj** %stackaddr$prim55688, align 8
%stackaddr$prim55689 = alloca %struct.ScmObj*, align 8
%argslist54259$k474432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49383, %struct.ScmObj* %argslist54259$k474431)
store volatile %struct.ScmObj* %argslist54259$k474432, %struct.ScmObj** %stackaddr$prim55689, align 8
%clofunc55690 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47443)
musttail call tailcc void %clofunc55690(%struct.ScmObj* %k47443, %struct.ScmObj* %argslist54259$k474432)
ret void
}

define tailcc void @proc_clo$ae49304(%struct.ScmObj* %env$ae49304,%struct.ScmObj* %current_45args54262) {
%stackaddr$env-ref55691 = alloca %struct.ScmObj*, align 8
%_37append47162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49304, i64 0)
store %struct.ScmObj* %_37append47162, %struct.ScmObj** %stackaddr$env-ref55691
%stackaddr$prim55692 = alloca %struct.ScmObj*, align 8
%k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54262)
store volatile %struct.ScmObj* %k47444, %struct.ScmObj** %stackaddr$prim55692, align 8
%stackaddr$prim55693 = alloca %struct.ScmObj*, align 8
%current_45args54263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54262)
store volatile %struct.ScmObj* %current_45args54263, %struct.ScmObj** %stackaddr$prim55693, align 8
%stackaddr$prim55694 = alloca %struct.ScmObj*, align 8
%ls047165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54263)
store volatile %struct.ScmObj* %ls047165, %struct.ScmObj** %stackaddr$prim55694, align 8
%stackaddr$prim55695 = alloca %struct.ScmObj*, align 8
%current_45args54264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54263)
store volatile %struct.ScmObj* %current_45args54264, %struct.ScmObj** %stackaddr$prim55695, align 8
%stackaddr$prim55696 = alloca %struct.ScmObj*, align 8
%ls147164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54264)
store volatile %struct.ScmObj* %ls147164, %struct.ScmObj** %stackaddr$prim55696, align 8
%stackaddr$prim55697 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047165)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim55697, align 8
%truthy$cmp55698 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47277)
%cmp$cmp55698 = icmp eq i64 %truthy$cmp55698, 1
br i1 %cmp$cmp55698, label %truebranch$cmp55698, label %falsebranch$cmp55698
truebranch$cmp55698:
%ae49308 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54266$k474440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55699 = alloca %struct.ScmObj*, align 8
%argslist54266$k474441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147164, %struct.ScmObj* %argslist54266$k474440)
store volatile %struct.ScmObj* %argslist54266$k474441, %struct.ScmObj** %stackaddr$prim55699, align 8
%stackaddr$prim55700 = alloca %struct.ScmObj*, align 8
%argslist54266$k474442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49308, %struct.ScmObj* %argslist54266$k474441)
store volatile %struct.ScmObj* %argslist54266$k474442, %struct.ScmObj** %stackaddr$prim55700, align 8
%clofunc55701 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47444)
musttail call tailcc void %clofunc55701(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist54266$k474442)
ret void
falsebranch$cmp55698:
%stackaddr$prim55702 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047165)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim55702, align 8
%ae49315 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55703 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47162, %struct.ScmObj* %ae49315)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim55703, align 8
%stackaddr$prim55704 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047165)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim55704, align 8
%stackaddr$makeclosure55705 = alloca %struct.ScmObj*, align 8
%fptrToInt55706 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49318 to i64
%ae49318 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55706)
store volatile %struct.ScmObj* %ae49318, %struct.ScmObj** %stackaddr$makeclosure55705, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49318, %struct.ScmObj* %k47444, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49318, %struct.ScmObj* %anf_45bind47278, i64 1)
%argslist54271$anf_45bind472790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55707 = alloca %struct.ScmObj*, align 8
%argslist54271$anf_45bind472791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147164, %struct.ScmObj* %argslist54271$anf_45bind472790)
store volatile %struct.ScmObj* %argslist54271$anf_45bind472791, %struct.ScmObj** %stackaddr$prim55707, align 8
%stackaddr$prim55708 = alloca %struct.ScmObj*, align 8
%argslist54271$anf_45bind472792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47280, %struct.ScmObj* %argslist54271$anf_45bind472791)
store volatile %struct.ScmObj* %argslist54271$anf_45bind472792, %struct.ScmObj** %stackaddr$prim55708, align 8
%stackaddr$prim55709 = alloca %struct.ScmObj*, align 8
%argslist54271$anf_45bind472793 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49318, %struct.ScmObj* %argslist54271$anf_45bind472792)
store volatile %struct.ScmObj* %argslist54271$anf_45bind472793, %struct.ScmObj** %stackaddr$prim55709, align 8
%clofunc55710 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47279)
musttail call tailcc void %clofunc55710(%struct.ScmObj* %anf_45bind47279, %struct.ScmObj* %argslist54271$anf_45bind472793)
ret void
}

define tailcc void @proc_clo$ae49318(%struct.ScmObj* %env$ae49318,%struct.ScmObj* %current_45args54267) {
%stackaddr$env-ref55711 = alloca %struct.ScmObj*, align 8
%k47444 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49318, i64 0)
store %struct.ScmObj* %k47444, %struct.ScmObj** %stackaddr$env-ref55711
%stackaddr$env-ref55712 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49318, i64 1)
store %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$env-ref55712
%stackaddr$prim55713 = alloca %struct.ScmObj*, align 8
%_95k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54267)
store volatile %struct.ScmObj* %_95k47445, %struct.ScmObj** %stackaddr$prim55713, align 8
%stackaddr$prim55714 = alloca %struct.ScmObj*, align 8
%current_45args54268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54267)
store volatile %struct.ScmObj* %current_45args54268, %struct.ScmObj** %stackaddr$prim55714, align 8
%stackaddr$prim55715 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54268)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim55715, align 8
%stackaddr$prim55716 = alloca %struct.ScmObj*, align 8
%cpsprim47446 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47278, %struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %cpsprim47446, %struct.ScmObj** %stackaddr$prim55716, align 8
%ae49324 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54270$k474440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55717 = alloca %struct.ScmObj*, align 8
%argslist54270$k474441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47446, %struct.ScmObj* %argslist54270$k474440)
store volatile %struct.ScmObj* %argslist54270$k474441, %struct.ScmObj** %stackaddr$prim55717, align 8
%stackaddr$prim55718 = alloca %struct.ScmObj*, align 8
%argslist54270$k474442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49324, %struct.ScmObj* %argslist54270$k474441)
store volatile %struct.ScmObj* %argslist54270$k474442, %struct.ScmObj** %stackaddr$prim55718, align 8
%clofunc55719 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47444)
musttail call tailcc void %clofunc55719(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist54270$k474442)
ret void
}

define tailcc void @proc_clo$ae49278(%struct.ScmObj* %env$ae49278,%struct.ScmObj* %current_45args54273) {
%stackaddr$prim55720 = alloca %struct.ScmObj*, align 8
%k47447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54273)
store volatile %struct.ScmObj* %k47447, %struct.ScmObj** %stackaddr$prim55720, align 8
%stackaddr$prim55721 = alloca %struct.ScmObj*, align 8
%current_45args54274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54273)
store volatile %struct.ScmObj* %current_45args54274, %struct.ScmObj** %stackaddr$prim55721, align 8
%stackaddr$prim55722 = alloca %struct.ScmObj*, align 8
%a47168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54274)
store volatile %struct.ScmObj* %a47168, %struct.ScmObj** %stackaddr$prim55722, align 8
%stackaddr$prim55723 = alloca %struct.ScmObj*, align 8
%current_45args54275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54274)
store volatile %struct.ScmObj* %current_45args54275, %struct.ScmObj** %stackaddr$prim55723, align 8
%stackaddr$prim55724 = alloca %struct.ScmObj*, align 8
%b47167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54275)
store volatile %struct.ScmObj* %b47167, %struct.ScmObj** %stackaddr$prim55724, align 8
%stackaddr$prim55725 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47168, %struct.ScmObj* %b47167)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim55725, align 8
%stackaddr$prim55726 = alloca %struct.ScmObj*, align 8
%cpsprim47448 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %cpsprim47448, %struct.ScmObj** %stackaddr$prim55726, align 8
%ae49283 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54277$k474470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55727 = alloca %struct.ScmObj*, align 8
%argslist54277$k474471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47448, %struct.ScmObj* %argslist54277$k474470)
store volatile %struct.ScmObj* %argslist54277$k474471, %struct.ScmObj** %stackaddr$prim55727, align 8
%stackaddr$prim55728 = alloca %struct.ScmObj*, align 8
%argslist54277$k474472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49283, %struct.ScmObj* %argslist54277$k474471)
store volatile %struct.ScmObj* %argslist54277$k474472, %struct.ScmObj** %stackaddr$prim55728, align 8
%clofunc55729 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47447)
musttail call tailcc void %clofunc55729(%struct.ScmObj* %k47447, %struct.ScmObj* %argslist54277$k474472)
ret void
}

define tailcc void @proc_clo$ae49254(%struct.ScmObj* %env$ae49254,%struct.ScmObj* %current_45args54279) {
%stackaddr$prim55730 = alloca %struct.ScmObj*, align 8
%k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54279)
store volatile %struct.ScmObj* %k47449, %struct.ScmObj** %stackaddr$prim55730, align 8
%stackaddr$prim55731 = alloca %struct.ScmObj*, align 8
%current_45args54280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54279)
store volatile %struct.ScmObj* %current_45args54280, %struct.ScmObj** %stackaddr$prim55731, align 8
%stackaddr$prim55732 = alloca %struct.ScmObj*, align 8
%a47171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54280)
store volatile %struct.ScmObj* %a47171, %struct.ScmObj** %stackaddr$prim55732, align 8
%stackaddr$prim55733 = alloca %struct.ScmObj*, align 8
%current_45args54281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54280)
store volatile %struct.ScmObj* %current_45args54281, %struct.ScmObj** %stackaddr$prim55733, align 8
%stackaddr$prim55734 = alloca %struct.ScmObj*, align 8
%b47170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54281)
store volatile %struct.ScmObj* %b47170, %struct.ScmObj** %stackaddr$prim55734, align 8
%stackaddr$prim55735 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47171, %struct.ScmObj* %b47170)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim55735, align 8
%stackaddr$prim55736 = alloca %struct.ScmObj*, align 8
%cpsprim47450 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %cpsprim47450, %struct.ScmObj** %stackaddr$prim55736, align 8
%ae49259 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54283$k474490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55737 = alloca %struct.ScmObj*, align 8
%argslist54283$k474491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47450, %struct.ScmObj* %argslist54283$k474490)
store volatile %struct.ScmObj* %argslist54283$k474491, %struct.ScmObj** %stackaddr$prim55737, align 8
%stackaddr$prim55738 = alloca %struct.ScmObj*, align 8
%argslist54283$k474492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49259, %struct.ScmObj* %argslist54283$k474491)
store volatile %struct.ScmObj* %argslist54283$k474492, %struct.ScmObj** %stackaddr$prim55738, align 8
%clofunc55739 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47449)
musttail call tailcc void %clofunc55739(%struct.ScmObj* %k47449, %struct.ScmObj* %argslist54283$k474492)
ret void
}

define tailcc void @proc_clo$ae48860(%struct.ScmObj* %env$ae48860,%struct.ScmObj* %current_45args54286) {
%stackaddr$env-ref55740 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48860, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55740
%stackaddr$env-ref55741 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48860, i64 1)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref55741
%stackaddr$env-ref55742 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48860, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55742
%stackaddr$prim55743 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54286)
store volatile %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$prim55743, align 8
%stackaddr$prim55744 = alloca %struct.ScmObj*, align 8
%current_45args54287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54286)
store volatile %struct.ScmObj* %current_45args54287, %struct.ScmObj** %stackaddr$prim55744, align 8
%stackaddr$prim55745 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54287)
store volatile %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$prim55745, align 8
%ae48862 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55746 = alloca %struct.ScmObj*, align 8
%fptrToInt55747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48863 to i64
%ae48863 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55747)
store volatile %struct.ScmObj* %ae48863, %struct.ScmObj** %stackaddr$makeclosure55746, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %_37map147121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %_37foldr47095, i64 3)
%argslist54344$k474510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55748 = alloca %struct.ScmObj*, align 8
%argslist54344$k474511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48863, %struct.ScmObj* %argslist54344$k474510)
store volatile %struct.ScmObj* %argslist54344$k474511, %struct.ScmObj** %stackaddr$prim55748, align 8
%stackaddr$prim55749 = alloca %struct.ScmObj*, align 8
%argslist54344$k474512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48862, %struct.ScmObj* %argslist54344$k474511)
store volatile %struct.ScmObj* %argslist54344$k474512, %struct.ScmObj** %stackaddr$prim55749, align 8
%clofunc55750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47451)
musttail call tailcc void %clofunc55750(%struct.ScmObj* %k47451, %struct.ScmObj* %argslist54344$k474512)
ret void
}

define tailcc void @proc_clo$ae48863(%struct.ScmObj* %env$ae48863,%struct.ScmObj* %args4717447452) {
%stackaddr$env-ref55751 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref55751
%stackaddr$env-ref55752 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55752
%stackaddr$env-ref55753 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 2)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref55753
%stackaddr$env-ref55754 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 3)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55754
%stackaddr$prim55755 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4717447452)
store volatile %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$prim55755, align 8
%stackaddr$prim55756 = alloca %struct.ScmObj*, align 8
%args47174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4717447452)
store volatile %struct.ScmObj* %args47174, %struct.ScmObj** %stackaddr$prim55756, align 8
%stackaddr$prim55757 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47174)
store volatile %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$prim55757, align 8
%stackaddr$prim55758 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47174)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim55758, align 8
%stackaddr$prim55759 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47263)
store volatile %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$prim55759, align 8
%stackaddr$prim55760 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47174)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim55760, align 8
%stackaddr$prim55761 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47264)
store volatile %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$prim55761, align 8
%stackaddr$makeclosure55762 = alloca %struct.ScmObj*, align 8
%fptrToInt55763 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48871 to i64
%ae48871 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55763)
store volatile %struct.ScmObj* %ae48871, %struct.ScmObj** %stackaddr$makeclosure55762, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48871, %struct.ScmObj* %lsts47175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48871, %struct.ScmObj* %_37foldr47095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48871, %struct.ScmObj* %_37foldl47173, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48871, %struct.ScmObj* %_37foldr147090, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48871, %struct.ScmObj* %_37map147121, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48871, %struct.ScmObj* %k47453, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48871, %struct.ScmObj* %f47177, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48871, %struct.ScmObj* %acc47176, i64 7)
%ae48872 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55764 = alloca %struct.ScmObj*, align 8
%fptrToInt55765 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48873 to i64
%ae48873 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55765)
store volatile %struct.ScmObj* %ae48873, %struct.ScmObj** %stackaddr$makeclosure55764, align 8
%argslist54343$ae488710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55766 = alloca %struct.ScmObj*, align 8
%argslist54343$ae488711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48873, %struct.ScmObj* %argslist54343$ae488710)
store volatile %struct.ScmObj* %argslist54343$ae488711, %struct.ScmObj** %stackaddr$prim55766, align 8
%stackaddr$prim55767 = alloca %struct.ScmObj*, align 8
%argslist54343$ae488712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48872, %struct.ScmObj* %argslist54343$ae488711)
store volatile %struct.ScmObj* %argslist54343$ae488712, %struct.ScmObj** %stackaddr$prim55767, align 8
%clofunc55768 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48871)
musttail call tailcc void %clofunc55768(%struct.ScmObj* %ae48871, %struct.ScmObj* %argslist54343$ae488712)
ret void
}

define tailcc void @proc_clo$ae48871(%struct.ScmObj* %env$ae48871,%struct.ScmObj* %current_45args54289) {
%stackaddr$env-ref55769 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48871, i64 0)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref55769
%stackaddr$env-ref55770 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48871, i64 1)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55770
%stackaddr$env-ref55771 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48871, i64 2)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref55771
%stackaddr$env-ref55772 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48871, i64 3)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55772
%stackaddr$env-ref55773 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48871, i64 4)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref55773
%stackaddr$env-ref55774 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48871, i64 5)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref55774
%stackaddr$env-ref55775 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48871, i64 6)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref55775
%stackaddr$env-ref55776 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48871, i64 7)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref55776
%stackaddr$prim55777 = alloca %struct.ScmObj*, align 8
%_95k47454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54289)
store volatile %struct.ScmObj* %_95k47454, %struct.ScmObj** %stackaddr$prim55777, align 8
%stackaddr$prim55778 = alloca %struct.ScmObj*, align 8
%current_45args54290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54289)
store volatile %struct.ScmObj* %current_45args54290, %struct.ScmObj** %stackaddr$prim55778, align 8
%stackaddr$prim55779 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54290)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim55779, align 8
%stackaddr$makeclosure55780 = alloca %struct.ScmObj*, align 8
%fptrToInt55781 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48903 to i64
%ae48903 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55781)
store volatile %struct.ScmObj* %ae48903, %struct.ScmObj** %stackaddr$makeclosure55780, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %lsts47175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %_37foldr47095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %_37foldl47173, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %_37map147121, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %k47453, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %f47177, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %acc47176, i64 6)
%ae48905 = call %struct.ScmObj* @const_init_false()
%argslist54336$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55782 = alloca %struct.ScmObj*, align 8
%argslist54336$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47175, %struct.ScmObj* %argslist54336$_37foldr1470900)
store volatile %struct.ScmObj* %argslist54336$_37foldr1470901, %struct.ScmObj** %stackaddr$prim55782, align 8
%stackaddr$prim55783 = alloca %struct.ScmObj*, align 8
%argslist54336$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48905, %struct.ScmObj* %argslist54336$_37foldr1470901)
store volatile %struct.ScmObj* %argslist54336$_37foldr1470902, %struct.ScmObj** %stackaddr$prim55783, align 8
%stackaddr$prim55784 = alloca %struct.ScmObj*, align 8
%argslist54336$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47265, %struct.ScmObj* %argslist54336$_37foldr1470902)
store volatile %struct.ScmObj* %argslist54336$_37foldr1470903, %struct.ScmObj** %stackaddr$prim55784, align 8
%stackaddr$prim55785 = alloca %struct.ScmObj*, align 8
%argslist54336$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48903, %struct.ScmObj* %argslist54336$_37foldr1470903)
store volatile %struct.ScmObj* %argslist54336$_37foldr1470904, %struct.ScmObj** %stackaddr$prim55785, align 8
%clofunc55786 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc55786(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist54336$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae48903(%struct.ScmObj* %env$ae48903,%struct.ScmObj* %current_45args54292) {
%stackaddr$env-ref55787 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 0)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref55787
%stackaddr$env-ref55788 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 1)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55788
%stackaddr$env-ref55789 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 2)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref55789
%stackaddr$env-ref55790 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 3)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref55790
%stackaddr$env-ref55791 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 4)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref55791
%stackaddr$env-ref55792 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 5)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref55792
%stackaddr$env-ref55793 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 6)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref55793
%stackaddr$prim55794 = alloca %struct.ScmObj*, align 8
%_95k47455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54292)
store volatile %struct.ScmObj* %_95k47455, %struct.ScmObj** %stackaddr$prim55794, align 8
%stackaddr$prim55795 = alloca %struct.ScmObj*, align 8
%current_45args54293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54292)
store volatile %struct.ScmObj* %current_45args54293, %struct.ScmObj** %stackaddr$prim55795, align 8
%stackaddr$prim55796 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54293)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim55796, align 8
%truthy$cmp55797 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47266)
%cmp$cmp55797 = icmp eq i64 %truthy$cmp55797, 1
br i1 %cmp$cmp55797, label %truebranch$cmp55797, label %falsebranch$cmp55797
truebranch$cmp55797:
%ae48914 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54295$k474530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55798 = alloca %struct.ScmObj*, align 8
%argslist54295$k474531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47176, %struct.ScmObj* %argslist54295$k474530)
store volatile %struct.ScmObj* %argslist54295$k474531, %struct.ScmObj** %stackaddr$prim55798, align 8
%stackaddr$prim55799 = alloca %struct.ScmObj*, align 8
%argslist54295$k474532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48914, %struct.ScmObj* %argslist54295$k474531)
store volatile %struct.ScmObj* %argslist54295$k474532, %struct.ScmObj** %stackaddr$prim55799, align 8
%clofunc55800 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47453)
musttail call tailcc void %clofunc55800(%struct.ScmObj* %k47453, %struct.ScmObj* %argslist54295$k474532)
ret void
falsebranch$cmp55797:
%stackaddr$makeclosure55801 = alloca %struct.ScmObj*, align 8
%fptrToInt55802 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48919 to i64
%ae48919 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55802)
store volatile %struct.ScmObj* %ae48919, %struct.ScmObj** %stackaddr$makeclosure55801, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48919, %struct.ScmObj* %lsts47175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48919, %struct.ScmObj* %_37foldr47095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48919, %struct.ScmObj* %_37foldl47173, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48919, %struct.ScmObj* %_37map147121, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48919, %struct.ScmObj* %k47453, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48919, %struct.ScmObj* %f47177, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48919, %struct.ScmObj* %acc47176, i64 6)
%ae48920 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55803 = alloca %struct.ScmObj*, align 8
%fptrToInt55804 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48921 to i64
%ae48921 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55804)
store volatile %struct.ScmObj* %ae48921, %struct.ScmObj** %stackaddr$makeclosure55803, align 8
%argslist54335$ae489190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55805 = alloca %struct.ScmObj*, align 8
%argslist54335$ae489191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48921, %struct.ScmObj* %argslist54335$ae489190)
store volatile %struct.ScmObj* %argslist54335$ae489191, %struct.ScmObj** %stackaddr$prim55805, align 8
%stackaddr$prim55806 = alloca %struct.ScmObj*, align 8
%argslist54335$ae489192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48920, %struct.ScmObj* %argslist54335$ae489191)
store volatile %struct.ScmObj* %argslist54335$ae489192, %struct.ScmObj** %stackaddr$prim55806, align 8
%clofunc55807 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48919)
musttail call tailcc void %clofunc55807(%struct.ScmObj* %ae48919, %struct.ScmObj* %argslist54335$ae489192)
ret void
}

define tailcc void @proc_clo$ae48919(%struct.ScmObj* %env$ae48919,%struct.ScmObj* %current_45args54296) {
%stackaddr$env-ref55808 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48919, i64 0)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref55808
%stackaddr$env-ref55809 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48919, i64 1)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55809
%stackaddr$env-ref55810 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48919, i64 2)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref55810
%stackaddr$env-ref55811 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48919, i64 3)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref55811
%stackaddr$env-ref55812 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48919, i64 4)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref55812
%stackaddr$env-ref55813 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48919, i64 5)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref55813
%stackaddr$env-ref55814 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48919, i64 6)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref55814
%stackaddr$prim55815 = alloca %struct.ScmObj*, align 8
%_95k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54296)
store volatile %struct.ScmObj* %_95k47456, %struct.ScmObj** %stackaddr$prim55815, align 8
%stackaddr$prim55816 = alloca %struct.ScmObj*, align 8
%current_45args54297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54296)
store volatile %struct.ScmObj* %current_45args54297, %struct.ScmObj** %stackaddr$prim55816, align 8
%stackaddr$prim55817 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54297)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim55817, align 8
%stackaddr$makeclosure55818 = alloca %struct.ScmObj*, align 8
%fptrToInt55819 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48940 to i64
%ae48940 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55819)
store volatile %struct.ScmObj* %ae48940, %struct.ScmObj** %stackaddr$makeclosure55818, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %lsts47175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %_37foldr47095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %_37foldl47173, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %_37map147121, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %k47453, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %f47177, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %acc47176, i64 6)
%argslist54330$_37map1471210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55820 = alloca %struct.ScmObj*, align 8
%argslist54330$_37map1471211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47175, %struct.ScmObj* %argslist54330$_37map1471210)
store volatile %struct.ScmObj* %argslist54330$_37map1471211, %struct.ScmObj** %stackaddr$prim55820, align 8
%stackaddr$prim55821 = alloca %struct.ScmObj*, align 8
%argslist54330$_37map1471212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47267, %struct.ScmObj* %argslist54330$_37map1471211)
store volatile %struct.ScmObj* %argslist54330$_37map1471212, %struct.ScmObj** %stackaddr$prim55821, align 8
%stackaddr$prim55822 = alloca %struct.ScmObj*, align 8
%argslist54330$_37map1471213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48940, %struct.ScmObj* %argslist54330$_37map1471212)
store volatile %struct.ScmObj* %argslist54330$_37map1471213, %struct.ScmObj** %stackaddr$prim55822, align 8
%clofunc55823 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147121)
musttail call tailcc void %clofunc55823(%struct.ScmObj* %_37map147121, %struct.ScmObj* %argslist54330$_37map1471213)
ret void
}

define tailcc void @proc_clo$ae48940(%struct.ScmObj* %env$ae48940,%struct.ScmObj* %current_45args54299) {
%stackaddr$env-ref55824 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 0)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref55824
%stackaddr$env-ref55825 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 1)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55825
%stackaddr$env-ref55826 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 2)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref55826
%stackaddr$env-ref55827 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 3)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref55827
%stackaddr$env-ref55828 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 4)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref55828
%stackaddr$env-ref55829 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 5)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref55829
%stackaddr$env-ref55830 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 6)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref55830
%stackaddr$prim55831 = alloca %struct.ScmObj*, align 8
%_95k47457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54299)
store volatile %struct.ScmObj* %_95k47457, %struct.ScmObj** %stackaddr$prim55831, align 8
%stackaddr$prim55832 = alloca %struct.ScmObj*, align 8
%current_45args54300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54299)
store volatile %struct.ScmObj* %current_45args54300, %struct.ScmObj** %stackaddr$prim55832, align 8
%stackaddr$prim55833 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54300)
store volatile %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$prim55833, align 8
%stackaddr$makeclosure55834 = alloca %struct.ScmObj*, align 8
%fptrToInt55835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48943 to i64
%ae48943 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55835)
store volatile %struct.ScmObj* %ae48943, %struct.ScmObj** %stackaddr$makeclosure55834, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %lsts47175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %_37foldr47095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %_37foldl47173, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %_37map147121, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %lsts_4347182, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %k47453, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %f47177, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %acc47176, i64 7)
%ae48944 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55836 = alloca %struct.ScmObj*, align 8
%fptrToInt55837 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48945 to i64
%ae48945 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55837)
store volatile %struct.ScmObj* %ae48945, %struct.ScmObj** %stackaddr$makeclosure55836, align 8
%argslist54329$ae489430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55838 = alloca %struct.ScmObj*, align 8
%argslist54329$ae489431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48945, %struct.ScmObj* %argslist54329$ae489430)
store volatile %struct.ScmObj* %argslist54329$ae489431, %struct.ScmObj** %stackaddr$prim55838, align 8
%stackaddr$prim55839 = alloca %struct.ScmObj*, align 8
%argslist54329$ae489432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48944, %struct.ScmObj* %argslist54329$ae489431)
store volatile %struct.ScmObj* %argslist54329$ae489432, %struct.ScmObj** %stackaddr$prim55839, align 8
%clofunc55840 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48943)
musttail call tailcc void %clofunc55840(%struct.ScmObj* %ae48943, %struct.ScmObj* %argslist54329$ae489432)
ret void
}

define tailcc void @proc_clo$ae48943(%struct.ScmObj* %env$ae48943,%struct.ScmObj* %current_45args54302) {
%stackaddr$env-ref55841 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 0)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref55841
%stackaddr$env-ref55842 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 1)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55842
%stackaddr$env-ref55843 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 2)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref55843
%stackaddr$env-ref55844 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 3)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref55844
%stackaddr$env-ref55845 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 4)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref55845
%stackaddr$env-ref55846 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 5)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref55846
%stackaddr$env-ref55847 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 6)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref55847
%stackaddr$env-ref55848 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 7)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref55848
%stackaddr$prim55849 = alloca %struct.ScmObj*, align 8
%_95k47458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54302)
store volatile %struct.ScmObj* %_95k47458, %struct.ScmObj** %stackaddr$prim55849, align 8
%stackaddr$prim55850 = alloca %struct.ScmObj*, align 8
%current_45args54303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54302)
store volatile %struct.ScmObj* %current_45args54303, %struct.ScmObj** %stackaddr$prim55850, align 8
%stackaddr$prim55851 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54303)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim55851, align 8
%stackaddr$makeclosure55852 = alloca %struct.ScmObj*, align 8
%fptrToInt55853 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48964 to i64
%ae48964 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55853)
store volatile %struct.ScmObj* %ae48964, %struct.ScmObj** %stackaddr$makeclosure55852, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48964, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48964, %struct.ScmObj* %lsts_4347182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48964, %struct.ScmObj* %k47453, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48964, %struct.ScmObj* %f47177, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48964, %struct.ScmObj* %acc47176, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48964, %struct.ScmObj* %_37foldr47095, i64 5)
%argslist54324$_37map1471210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55854 = alloca %struct.ScmObj*, align 8
%argslist54324$_37map1471211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47175, %struct.ScmObj* %argslist54324$_37map1471210)
store volatile %struct.ScmObj* %argslist54324$_37map1471211, %struct.ScmObj** %stackaddr$prim55854, align 8
%stackaddr$prim55855 = alloca %struct.ScmObj*, align 8
%argslist54324$_37map1471212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47268, %struct.ScmObj* %argslist54324$_37map1471211)
store volatile %struct.ScmObj* %argslist54324$_37map1471212, %struct.ScmObj** %stackaddr$prim55855, align 8
%stackaddr$prim55856 = alloca %struct.ScmObj*, align 8
%argslist54324$_37map1471213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48964, %struct.ScmObj* %argslist54324$_37map1471212)
store volatile %struct.ScmObj* %argslist54324$_37map1471213, %struct.ScmObj** %stackaddr$prim55856, align 8
%clofunc55857 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147121)
musttail call tailcc void %clofunc55857(%struct.ScmObj* %_37map147121, %struct.ScmObj* %argslist54324$_37map1471213)
ret void
}

define tailcc void @proc_clo$ae48964(%struct.ScmObj* %env$ae48964,%struct.ScmObj* %current_45args54305) {
%stackaddr$env-ref55858 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48964, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref55858
%stackaddr$env-ref55859 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48964, i64 1)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref55859
%stackaddr$env-ref55860 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48964, i64 2)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref55860
%stackaddr$env-ref55861 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48964, i64 3)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref55861
%stackaddr$env-ref55862 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48964, i64 4)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref55862
%stackaddr$env-ref55863 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48964, i64 5)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55863
%stackaddr$prim55864 = alloca %struct.ScmObj*, align 8
%_95k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54305)
store volatile %struct.ScmObj* %_95k47459, %struct.ScmObj** %stackaddr$prim55864, align 8
%stackaddr$prim55865 = alloca %struct.ScmObj*, align 8
%current_45args54306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54305)
store volatile %struct.ScmObj* %current_45args54306, %struct.ScmObj** %stackaddr$prim55865, align 8
%stackaddr$prim55866 = alloca %struct.ScmObj*, align 8
%vs47180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54306)
store volatile %struct.ScmObj* %vs47180, %struct.ScmObj** %stackaddr$prim55866, align 8
%stackaddr$makeclosure55867 = alloca %struct.ScmObj*, align 8
%fptrToInt55868 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48967 to i64
%ae48967 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55868)
store volatile %struct.ScmObj* %ae48967, %struct.ScmObj** %stackaddr$makeclosure55867, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %lsts_4347182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %k47453, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %vs47180, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %f47177, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %acc47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48968 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55869 = alloca %struct.ScmObj*, align 8
%fptrToInt55870 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48969 to i64
%ae48969 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55870)
store volatile %struct.ScmObj* %ae48969, %struct.ScmObj** %stackaddr$makeclosure55869, align 8
%argslist54323$ae489670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55871 = alloca %struct.ScmObj*, align 8
%argslist54323$ae489671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48969, %struct.ScmObj* %argslist54323$ae489670)
store volatile %struct.ScmObj* %argslist54323$ae489671, %struct.ScmObj** %stackaddr$prim55871, align 8
%stackaddr$prim55872 = alloca %struct.ScmObj*, align 8
%argslist54323$ae489672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48968, %struct.ScmObj* %argslist54323$ae489671)
store volatile %struct.ScmObj* %argslist54323$ae489672, %struct.ScmObj** %stackaddr$prim55872, align 8
%clofunc55873 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48967)
musttail call tailcc void %clofunc55873(%struct.ScmObj* %ae48967, %struct.ScmObj* %argslist54323$ae489672)
ret void
}

define tailcc void @proc_clo$ae48967(%struct.ScmObj* %env$ae48967,%struct.ScmObj* %current_45args54308) {
%stackaddr$env-ref55874 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref55874
%stackaddr$env-ref55875 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 1)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref55875
%stackaddr$env-ref55876 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 2)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref55876
%stackaddr$env-ref55877 = alloca %struct.ScmObj*, align 8
%vs47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 3)
store %struct.ScmObj* %vs47180, %struct.ScmObj** %stackaddr$env-ref55877
%stackaddr$env-ref55878 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 4)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref55878
%stackaddr$env-ref55879 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 5)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref55879
%stackaddr$env-ref55880 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55880
%stackaddr$prim55881 = alloca %struct.ScmObj*, align 8
%_95k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54308)
store volatile %struct.ScmObj* %_95k47460, %struct.ScmObj** %stackaddr$prim55881, align 8
%stackaddr$prim55882 = alloca %struct.ScmObj*, align 8
%current_45args54309 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54308)
store volatile %struct.ScmObj* %current_45args54309, %struct.ScmObj** %stackaddr$prim55882, align 8
%stackaddr$prim55883 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54309)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim55883, align 8
%ae48990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55884 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47176, %struct.ScmObj* %ae48990)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim55884, align 8
%stackaddr$makeclosure55885 = alloca %struct.ScmObj*, align 8
%fptrToInt55886 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48992 to i64
%ae48992 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55886)
store volatile %struct.ScmObj* %ae48992, %struct.ScmObj** %stackaddr$makeclosure55885, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48992, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48992, %struct.ScmObj* %lsts_4347182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48992, %struct.ScmObj* %k47453, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48992, %struct.ScmObj* %f47177, i64 3)
%argslist54317$_37foldr470950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55887 = alloca %struct.ScmObj*, align 8
%argslist54317$_37foldr470951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47180, %struct.ScmObj* %argslist54317$_37foldr470950)
store volatile %struct.ScmObj* %argslist54317$_37foldr470951, %struct.ScmObj** %stackaddr$prim55887, align 8
%stackaddr$prim55888 = alloca %struct.ScmObj*, align 8
%argslist54317$_37foldr470952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47270, %struct.ScmObj* %argslist54317$_37foldr470951)
store volatile %struct.ScmObj* %argslist54317$_37foldr470952, %struct.ScmObj** %stackaddr$prim55888, align 8
%stackaddr$prim55889 = alloca %struct.ScmObj*, align 8
%argslist54317$_37foldr470953 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47269, %struct.ScmObj* %argslist54317$_37foldr470952)
store volatile %struct.ScmObj* %argslist54317$_37foldr470953, %struct.ScmObj** %stackaddr$prim55889, align 8
%stackaddr$prim55890 = alloca %struct.ScmObj*, align 8
%argslist54317$_37foldr470954 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48992, %struct.ScmObj* %argslist54317$_37foldr470953)
store volatile %struct.ScmObj* %argslist54317$_37foldr470954, %struct.ScmObj** %stackaddr$prim55890, align 8
%clofunc55891 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47095)
musttail call tailcc void %clofunc55891(%struct.ScmObj* %_37foldr47095, %struct.ScmObj* %argslist54317$_37foldr470954)
ret void
}

define tailcc void @proc_clo$ae48992(%struct.ScmObj* %env$ae48992,%struct.ScmObj* %current_45args54311) {
%stackaddr$env-ref55892 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48992, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref55892
%stackaddr$env-ref55893 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48992, i64 1)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref55893
%stackaddr$env-ref55894 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48992, i64 2)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref55894
%stackaddr$env-ref55895 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48992, i64 3)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref55895
%stackaddr$prim55896 = alloca %struct.ScmObj*, align 8
%_95k47461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54311)
store volatile %struct.ScmObj* %_95k47461, %struct.ScmObj** %stackaddr$prim55896, align 8
%stackaddr$prim55897 = alloca %struct.ScmObj*, align 8
%current_45args54312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54311)
store volatile %struct.ScmObj* %current_45args54312, %struct.ScmObj** %stackaddr$prim55897, align 8
%stackaddr$prim55898 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54312)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim55898, align 8
%stackaddr$makeclosure55899 = alloca %struct.ScmObj*, align 8
%fptrToInt55900 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48996 to i64
%ae48996 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55900)
store volatile %struct.ScmObj* %ae48996, %struct.ScmObj** %stackaddr$makeclosure55899, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48996, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48996, %struct.ScmObj* %lsts_4347182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48996, %struct.ScmObj* %k47453, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48996, %struct.ScmObj* %f47177, i64 3)
%stackaddr$prim55901 = alloca %struct.ScmObj*, align 8
%cpsargs47464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48996, %struct.ScmObj* %anf_45bind47271)
store volatile %struct.ScmObj* %cpsargs47464, %struct.ScmObj** %stackaddr$prim55901, align 8
%clofunc55902 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47177)
musttail call tailcc void %clofunc55902(%struct.ScmObj* %f47177, %struct.ScmObj* %cpsargs47464)
ret void
}

define tailcc void @proc_clo$ae48996(%struct.ScmObj* %env$ae48996,%struct.ScmObj* %current_45args54314) {
%stackaddr$env-ref55903 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48996, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref55903
%stackaddr$env-ref55904 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48996, i64 1)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref55904
%stackaddr$env-ref55905 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48996, i64 2)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref55905
%stackaddr$env-ref55906 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48996, i64 3)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref55906
%stackaddr$prim55907 = alloca %struct.ScmObj*, align 8
%_95k47462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54314)
store volatile %struct.ScmObj* %_95k47462, %struct.ScmObj** %stackaddr$prim55907, align 8
%stackaddr$prim55908 = alloca %struct.ScmObj*, align 8
%current_45args54315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54314)
store volatile %struct.ScmObj* %current_45args54315, %struct.ScmObj** %stackaddr$prim55908, align 8
%stackaddr$prim55909 = alloca %struct.ScmObj*, align 8
%acc_4347184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54315)
store volatile %struct.ScmObj* %acc_4347184, %struct.ScmObj** %stackaddr$prim55909, align 8
%stackaddr$prim55910 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347184, %struct.ScmObj* %lsts_4347182)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim55910, align 8
%stackaddr$prim55911 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47177, %struct.ScmObj* %anf_45bind47272)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim55911, align 8
%stackaddr$prim55912 = alloca %struct.ScmObj*, align 8
%cpsargs47463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47453, %struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %cpsargs47463, %struct.ScmObj** %stackaddr$prim55912, align 8
%clofunc55913 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47173)
musttail call tailcc void %clofunc55913(%struct.ScmObj* %_37foldl47173, %struct.ScmObj* %cpsargs47463)
ret void
}

define tailcc void @proc_clo$ae48969(%struct.ScmObj* %env$ae48969,%struct.ScmObj* %current_45args54318) {
%stackaddr$prim55914 = alloca %struct.ScmObj*, align 8
%k47465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54318)
store volatile %struct.ScmObj* %k47465, %struct.ScmObj** %stackaddr$prim55914, align 8
%stackaddr$prim55915 = alloca %struct.ScmObj*, align 8
%current_45args54319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54318)
store volatile %struct.ScmObj* %current_45args54319, %struct.ScmObj** %stackaddr$prim55915, align 8
%stackaddr$prim55916 = alloca %struct.ScmObj*, align 8
%a47186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54319)
store volatile %struct.ScmObj* %a47186, %struct.ScmObj** %stackaddr$prim55916, align 8
%stackaddr$prim55917 = alloca %struct.ScmObj*, align 8
%current_45args54320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54319)
store volatile %struct.ScmObj* %current_45args54320, %struct.ScmObj** %stackaddr$prim55917, align 8
%stackaddr$prim55918 = alloca %struct.ScmObj*, align 8
%b47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54320)
store volatile %struct.ScmObj* %b47185, %struct.ScmObj** %stackaddr$prim55918, align 8
%stackaddr$prim55919 = alloca %struct.ScmObj*, align 8
%cpsprim47466 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47186, %struct.ScmObj* %b47185)
store volatile %struct.ScmObj* %cpsprim47466, %struct.ScmObj** %stackaddr$prim55919, align 8
%ae48973 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54322$k474650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55920 = alloca %struct.ScmObj*, align 8
%argslist54322$k474651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47466, %struct.ScmObj* %argslist54322$k474650)
store volatile %struct.ScmObj* %argslist54322$k474651, %struct.ScmObj** %stackaddr$prim55920, align 8
%stackaddr$prim55921 = alloca %struct.ScmObj*, align 8
%argslist54322$k474652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48973, %struct.ScmObj* %argslist54322$k474651)
store volatile %struct.ScmObj* %argslist54322$k474652, %struct.ScmObj** %stackaddr$prim55921, align 8
%clofunc55922 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47465)
musttail call tailcc void %clofunc55922(%struct.ScmObj* %k47465, %struct.ScmObj* %argslist54322$k474652)
ret void
}

define tailcc void @proc_clo$ae48945(%struct.ScmObj* %env$ae48945,%struct.ScmObj* %current_45args54325) {
%stackaddr$prim55923 = alloca %struct.ScmObj*, align 8
%k47467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54325)
store volatile %struct.ScmObj* %k47467, %struct.ScmObj** %stackaddr$prim55923, align 8
%stackaddr$prim55924 = alloca %struct.ScmObj*, align 8
%current_45args54326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54325)
store volatile %struct.ScmObj* %current_45args54326, %struct.ScmObj** %stackaddr$prim55924, align 8
%stackaddr$prim55925 = alloca %struct.ScmObj*, align 8
%x47181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54326)
store volatile %struct.ScmObj* %x47181, %struct.ScmObj** %stackaddr$prim55925, align 8
%stackaddr$prim55926 = alloca %struct.ScmObj*, align 8
%cpsprim47468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47181)
store volatile %struct.ScmObj* %cpsprim47468, %struct.ScmObj** %stackaddr$prim55926, align 8
%ae48948 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54328$k474670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55927 = alloca %struct.ScmObj*, align 8
%argslist54328$k474671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47468, %struct.ScmObj* %argslist54328$k474670)
store volatile %struct.ScmObj* %argslist54328$k474671, %struct.ScmObj** %stackaddr$prim55927, align 8
%stackaddr$prim55928 = alloca %struct.ScmObj*, align 8
%argslist54328$k474672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48948, %struct.ScmObj* %argslist54328$k474671)
store volatile %struct.ScmObj* %argslist54328$k474672, %struct.ScmObj** %stackaddr$prim55928, align 8
%clofunc55929 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47467)
musttail call tailcc void %clofunc55929(%struct.ScmObj* %k47467, %struct.ScmObj* %argslist54328$k474672)
ret void
}

define tailcc void @proc_clo$ae48921(%struct.ScmObj* %env$ae48921,%struct.ScmObj* %current_45args54331) {
%stackaddr$prim55930 = alloca %struct.ScmObj*, align 8
%k47469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54331)
store volatile %struct.ScmObj* %k47469, %struct.ScmObj** %stackaddr$prim55930, align 8
%stackaddr$prim55931 = alloca %struct.ScmObj*, align 8
%current_45args54332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54331)
store volatile %struct.ScmObj* %current_45args54332, %struct.ScmObj** %stackaddr$prim55931, align 8
%stackaddr$prim55932 = alloca %struct.ScmObj*, align 8
%x47183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54332)
store volatile %struct.ScmObj* %x47183, %struct.ScmObj** %stackaddr$prim55932, align 8
%stackaddr$prim55933 = alloca %struct.ScmObj*, align 8
%cpsprim47470 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47183)
store volatile %struct.ScmObj* %cpsprim47470, %struct.ScmObj** %stackaddr$prim55933, align 8
%ae48924 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54334$k474690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55934 = alloca %struct.ScmObj*, align 8
%argslist54334$k474691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47470, %struct.ScmObj* %argslist54334$k474690)
store volatile %struct.ScmObj* %argslist54334$k474691, %struct.ScmObj** %stackaddr$prim55934, align 8
%stackaddr$prim55935 = alloca %struct.ScmObj*, align 8
%argslist54334$k474692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48924, %struct.ScmObj* %argslist54334$k474691)
store volatile %struct.ScmObj* %argslist54334$k474692, %struct.ScmObj** %stackaddr$prim55935, align 8
%clofunc55936 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47469)
musttail call tailcc void %clofunc55936(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist54334$k474692)
ret void
}

define tailcc void @proc_clo$ae48873(%struct.ScmObj* %env$ae48873,%struct.ScmObj* %current_45args54337) {
%stackaddr$prim55937 = alloca %struct.ScmObj*, align 8
%k47471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54337)
store volatile %struct.ScmObj* %k47471, %struct.ScmObj** %stackaddr$prim55937, align 8
%stackaddr$prim55938 = alloca %struct.ScmObj*, align 8
%current_45args54338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54337)
store volatile %struct.ScmObj* %current_45args54338, %struct.ScmObj** %stackaddr$prim55938, align 8
%stackaddr$prim55939 = alloca %struct.ScmObj*, align 8
%lst47179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54338)
store volatile %struct.ScmObj* %lst47179, %struct.ScmObj** %stackaddr$prim55939, align 8
%stackaddr$prim55940 = alloca %struct.ScmObj*, align 8
%current_45args54339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54338)
store volatile %struct.ScmObj* %current_45args54339, %struct.ScmObj** %stackaddr$prim55940, align 8
%stackaddr$prim55941 = alloca %struct.ScmObj*, align 8
%b47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54339)
store volatile %struct.ScmObj* %b47178, %struct.ScmObj** %stackaddr$prim55941, align 8
%truthy$cmp55942 = call i64 @is_truthy_value(%struct.ScmObj* %b47178)
%cmp$cmp55942 = icmp eq i64 %truthy$cmp55942, 1
br i1 %cmp$cmp55942, label %truebranch$cmp55942, label %falsebranch$cmp55942
truebranch$cmp55942:
%ae48876 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54341$k474710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55943 = alloca %struct.ScmObj*, align 8
%argslist54341$k474711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47178, %struct.ScmObj* %argslist54341$k474710)
store volatile %struct.ScmObj* %argslist54341$k474711, %struct.ScmObj** %stackaddr$prim55943, align 8
%stackaddr$prim55944 = alloca %struct.ScmObj*, align 8
%argslist54341$k474712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48876, %struct.ScmObj* %argslist54341$k474711)
store volatile %struct.ScmObj* %argslist54341$k474712, %struct.ScmObj** %stackaddr$prim55944, align 8
%clofunc55945 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47471)
musttail call tailcc void %clofunc55945(%struct.ScmObj* %k47471, %struct.ScmObj* %argslist54341$k474712)
ret void
falsebranch$cmp55942:
%stackaddr$prim55946 = alloca %struct.ScmObj*, align 8
%cpsprim47472 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47179)
store volatile %struct.ScmObj* %cpsprim47472, %struct.ScmObj** %stackaddr$prim55946, align 8
%ae48883 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54342$k474710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55947 = alloca %struct.ScmObj*, align 8
%argslist54342$k474711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47472, %struct.ScmObj* %argslist54342$k474710)
store volatile %struct.ScmObj* %argslist54342$k474711, %struct.ScmObj** %stackaddr$prim55947, align 8
%stackaddr$prim55948 = alloca %struct.ScmObj*, align 8
%argslist54342$k474712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48883, %struct.ScmObj* %argslist54342$k474711)
store volatile %struct.ScmObj* %argslist54342$k474712, %struct.ScmObj** %stackaddr$prim55948, align 8
%clofunc55949 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47471)
musttail call tailcc void %clofunc55949(%struct.ScmObj* %k47471, %struct.ScmObj* %argslist54342$k474712)
ret void
}

define tailcc void @proc_clo$ae48714(%struct.ScmObj* %env$ae48714,%struct.ScmObj* %args4711747473) {
%stackaddr$env-ref55950 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48714, i64 0)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref55950
%stackaddr$env-ref55951 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48714, i64 1)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref55951
%stackaddr$env-ref55952 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48714, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55952
%stackaddr$prim55953 = alloca %struct.ScmObj*, align 8
%k47474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4711747473)
store volatile %struct.ScmObj* %k47474, %struct.ScmObj** %stackaddr$prim55953, align 8
%stackaddr$prim55954 = alloca %struct.ScmObj*, align 8
%args47117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4711747473)
store volatile %struct.ScmObj* %args47117, %struct.ScmObj** %stackaddr$prim55954, align 8
%stackaddr$prim55955 = alloca %struct.ScmObj*, align 8
%f47119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47117)
store volatile %struct.ScmObj* %f47119, %struct.ScmObj** %stackaddr$prim55955, align 8
%stackaddr$prim55956 = alloca %struct.ScmObj*, align 8
%lsts47118 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47117)
store volatile %struct.ScmObj* %lsts47118, %struct.ScmObj** %stackaddr$prim55956, align 8
%stackaddr$makeclosure55957 = alloca %struct.ScmObj*, align 8
%fptrToInt55958 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48719 to i64
%ae48719 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55958)
store volatile %struct.ScmObj* %ae48719, %struct.ScmObj** %stackaddr$makeclosure55957, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48719, %struct.ScmObj* %k47474, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48719, %struct.ScmObj* %lsts47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48719, %struct.ScmObj* %_37foldr47095, i64 2)
%ae48720 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55959 = alloca %struct.ScmObj*, align 8
%fptrToInt55960 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48721 to i64
%ae48721 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55960)
store volatile %struct.ScmObj* %ae48721, %struct.ScmObj** %stackaddr$makeclosure55959, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48721, %struct.ScmObj* %_37drop_45right47109, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48721, %struct.ScmObj* %f47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48721, %struct.ScmObj* %_37last47112, i64 2)
%argslist54361$ae487190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55961 = alloca %struct.ScmObj*, align 8
%argslist54361$ae487191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48721, %struct.ScmObj* %argslist54361$ae487190)
store volatile %struct.ScmObj* %argslist54361$ae487191, %struct.ScmObj** %stackaddr$prim55961, align 8
%stackaddr$prim55962 = alloca %struct.ScmObj*, align 8
%argslist54361$ae487192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48720, %struct.ScmObj* %argslist54361$ae487191)
store volatile %struct.ScmObj* %argslist54361$ae487192, %struct.ScmObj** %stackaddr$prim55962, align 8
%clofunc55963 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48719)
musttail call tailcc void %clofunc55963(%struct.ScmObj* %ae48719, %struct.ScmObj* %argslist54361$ae487192)
ret void
}

define tailcc void @proc_clo$ae48719(%struct.ScmObj* %env$ae48719,%struct.ScmObj* %current_45args54346) {
%stackaddr$env-ref55964 = alloca %struct.ScmObj*, align 8
%k47474 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48719, i64 0)
store %struct.ScmObj* %k47474, %struct.ScmObj** %stackaddr$env-ref55964
%stackaddr$env-ref55965 = alloca %struct.ScmObj*, align 8
%lsts47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48719, i64 1)
store %struct.ScmObj* %lsts47118, %struct.ScmObj** %stackaddr$env-ref55965
%stackaddr$env-ref55966 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48719, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55966
%stackaddr$prim55967 = alloca %struct.ScmObj*, align 8
%_95k47475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54346)
store volatile %struct.ScmObj* %_95k47475, %struct.ScmObj** %stackaddr$prim55967, align 8
%stackaddr$prim55968 = alloca %struct.ScmObj*, align 8
%current_45args54347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54346)
store volatile %struct.ScmObj* %current_45args54347, %struct.ScmObj** %stackaddr$prim55968, align 8
%stackaddr$prim55969 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54347)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim55969, align 8
%ae48782 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55970 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48782, %struct.ScmObj* %lsts47118)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim55970, align 8
%stackaddr$prim55971 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47260, %struct.ScmObj* %anf_45bind47261)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim55971, align 8
%stackaddr$prim55972 = alloca %struct.ScmObj*, align 8
%cpsargs47476 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47474, %struct.ScmObj* %anf_45bind47262)
store volatile %struct.ScmObj* %cpsargs47476, %struct.ScmObj** %stackaddr$prim55972, align 8
%clofunc55973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47095)
musttail call tailcc void %clofunc55973(%struct.ScmObj* %_37foldr47095, %struct.ScmObj* %cpsargs47476)
ret void
}

define tailcc void @proc_clo$ae48721(%struct.ScmObj* %env$ae48721,%struct.ScmObj* %fargs4712047477) {
%stackaddr$env-ref55974 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48721, i64 0)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref55974
%stackaddr$env-ref55975 = alloca %struct.ScmObj*, align 8
%f47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48721, i64 1)
store %struct.ScmObj* %f47119, %struct.ScmObj** %stackaddr$env-ref55975
%stackaddr$env-ref55976 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48721, i64 2)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref55976
%stackaddr$prim55977 = alloca %struct.ScmObj*, align 8
%k47478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4712047477)
store volatile %struct.ScmObj* %k47478, %struct.ScmObj** %stackaddr$prim55977, align 8
%stackaddr$prim55978 = alloca %struct.ScmObj*, align 8
%fargs47120 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4712047477)
store volatile %struct.ScmObj* %fargs47120, %struct.ScmObj** %stackaddr$prim55978, align 8
%stackaddr$makeclosure55979 = alloca %struct.ScmObj*, align 8
%fptrToInt55980 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48725 to i64
%ae48725 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55980)
store volatile %struct.ScmObj* %ae48725, %struct.ScmObj** %stackaddr$makeclosure55979, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48725, %struct.ScmObj* %k47478, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48725, %struct.ScmObj* %fargs47120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48725, %struct.ScmObj* %f47119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48725, %struct.ScmObj* %_37last47112, i64 3)
%ae48727 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54360$_37drop_45right471090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55981 = alloca %struct.ScmObj*, align 8
%argslist54360$_37drop_45right471091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48727, %struct.ScmObj* %argslist54360$_37drop_45right471090)
store volatile %struct.ScmObj* %argslist54360$_37drop_45right471091, %struct.ScmObj** %stackaddr$prim55981, align 8
%stackaddr$prim55982 = alloca %struct.ScmObj*, align 8
%argslist54360$_37drop_45right471092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47120, %struct.ScmObj* %argslist54360$_37drop_45right471091)
store volatile %struct.ScmObj* %argslist54360$_37drop_45right471092, %struct.ScmObj** %stackaddr$prim55982, align 8
%stackaddr$prim55983 = alloca %struct.ScmObj*, align 8
%argslist54360$_37drop_45right471093 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48725, %struct.ScmObj* %argslist54360$_37drop_45right471092)
store volatile %struct.ScmObj* %argslist54360$_37drop_45right471093, %struct.ScmObj** %stackaddr$prim55983, align 8
%clofunc55984 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47109)
musttail call tailcc void %clofunc55984(%struct.ScmObj* %_37drop_45right47109, %struct.ScmObj* %argslist54360$_37drop_45right471093)
ret void
}

define tailcc void @proc_clo$ae48725(%struct.ScmObj* %env$ae48725,%struct.ScmObj* %current_45args54349) {
%stackaddr$env-ref55985 = alloca %struct.ScmObj*, align 8
%k47478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48725, i64 0)
store %struct.ScmObj* %k47478, %struct.ScmObj** %stackaddr$env-ref55985
%stackaddr$env-ref55986 = alloca %struct.ScmObj*, align 8
%fargs47120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48725, i64 1)
store %struct.ScmObj* %fargs47120, %struct.ScmObj** %stackaddr$env-ref55986
%stackaddr$env-ref55987 = alloca %struct.ScmObj*, align 8
%f47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48725, i64 2)
store %struct.ScmObj* %f47119, %struct.ScmObj** %stackaddr$env-ref55987
%stackaddr$env-ref55988 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48725, i64 3)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref55988
%stackaddr$prim55989 = alloca %struct.ScmObj*, align 8
%_95k47479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54349)
store volatile %struct.ScmObj* %_95k47479, %struct.ScmObj** %stackaddr$prim55989, align 8
%stackaddr$prim55990 = alloca %struct.ScmObj*, align 8
%current_45args54350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54349)
store volatile %struct.ScmObj* %current_45args54350, %struct.ScmObj** %stackaddr$prim55990, align 8
%stackaddr$prim55991 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54350)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim55991, align 8
%stackaddr$makeclosure55992 = alloca %struct.ScmObj*, align 8
%fptrToInt55993 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48732 to i64
%ae48732 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55993)
store volatile %struct.ScmObj* %ae48732, %struct.ScmObj** %stackaddr$makeclosure55992, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48732, %struct.ScmObj* %k47478, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48732, %struct.ScmObj* %fargs47120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48732, %struct.ScmObj* %_37last47112, i64 2)
%stackaddr$prim55994 = alloca %struct.ScmObj*, align 8
%cpsargs47483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48732, %struct.ScmObj* %anf_45bind47257)
store volatile %struct.ScmObj* %cpsargs47483, %struct.ScmObj** %stackaddr$prim55994, align 8
%clofunc55995 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47119)
musttail call tailcc void %clofunc55995(%struct.ScmObj* %f47119, %struct.ScmObj* %cpsargs47483)
ret void
}

define tailcc void @proc_clo$ae48732(%struct.ScmObj* %env$ae48732,%struct.ScmObj* %current_45args54352) {
%stackaddr$env-ref55996 = alloca %struct.ScmObj*, align 8
%k47478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48732, i64 0)
store %struct.ScmObj* %k47478, %struct.ScmObj** %stackaddr$env-ref55996
%stackaddr$env-ref55997 = alloca %struct.ScmObj*, align 8
%fargs47120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48732, i64 1)
store %struct.ScmObj* %fargs47120, %struct.ScmObj** %stackaddr$env-ref55997
%stackaddr$env-ref55998 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48732, i64 2)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref55998
%stackaddr$prim55999 = alloca %struct.ScmObj*, align 8
%_95k47480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54352)
store volatile %struct.ScmObj* %_95k47480, %struct.ScmObj** %stackaddr$prim55999, align 8
%stackaddr$prim56000 = alloca %struct.ScmObj*, align 8
%current_45args54353 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54352)
store volatile %struct.ScmObj* %current_45args54353, %struct.ScmObj** %stackaddr$prim56000, align 8
%stackaddr$prim56001 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54353)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim56001, align 8
%stackaddr$makeclosure56002 = alloca %struct.ScmObj*, align 8
%fptrToInt56003 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48737 to i64
%ae48737 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56003)
store volatile %struct.ScmObj* %ae48737, %struct.ScmObj** %stackaddr$makeclosure56002, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48737, %struct.ScmObj* %k47478, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48737, %struct.ScmObj* %anf_45bind47258, i64 1)
%argslist54359$_37last471120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56004 = alloca %struct.ScmObj*, align 8
%argslist54359$_37last471121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47120, %struct.ScmObj* %argslist54359$_37last471120)
store volatile %struct.ScmObj* %argslist54359$_37last471121, %struct.ScmObj** %stackaddr$prim56004, align 8
%stackaddr$prim56005 = alloca %struct.ScmObj*, align 8
%argslist54359$_37last471122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48737, %struct.ScmObj* %argslist54359$_37last471121)
store volatile %struct.ScmObj* %argslist54359$_37last471122, %struct.ScmObj** %stackaddr$prim56005, align 8
%clofunc56006 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47112)
musttail call tailcc void %clofunc56006(%struct.ScmObj* %_37last47112, %struct.ScmObj* %argslist54359$_37last471122)
ret void
}

define tailcc void @proc_clo$ae48737(%struct.ScmObj* %env$ae48737,%struct.ScmObj* %current_45args54355) {
%stackaddr$env-ref56007 = alloca %struct.ScmObj*, align 8
%k47478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48737, i64 0)
store %struct.ScmObj* %k47478, %struct.ScmObj** %stackaddr$env-ref56007
%stackaddr$env-ref56008 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48737, i64 1)
store %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$env-ref56008
%stackaddr$prim56009 = alloca %struct.ScmObj*, align 8
%_95k47481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54355)
store volatile %struct.ScmObj* %_95k47481, %struct.ScmObj** %stackaddr$prim56009, align 8
%stackaddr$prim56010 = alloca %struct.ScmObj*, align 8
%current_45args54356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54355)
store volatile %struct.ScmObj* %current_45args54356, %struct.ScmObj** %stackaddr$prim56010, align 8
%stackaddr$prim56011 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54356)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim56011, align 8
%stackaddr$prim56012 = alloca %struct.ScmObj*, align 8
%cpsprim47482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47258, %struct.ScmObj* %anf_45bind47259)
store volatile %struct.ScmObj* %cpsprim47482, %struct.ScmObj** %stackaddr$prim56012, align 8
%ae48742 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54358$k474780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56013 = alloca %struct.ScmObj*, align 8
%argslist54358$k474781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47482, %struct.ScmObj* %argslist54358$k474780)
store volatile %struct.ScmObj* %argslist54358$k474781, %struct.ScmObj** %stackaddr$prim56013, align 8
%stackaddr$prim56014 = alloca %struct.ScmObj*, align 8
%argslist54358$k474782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48742, %struct.ScmObj* %argslist54358$k474781)
store volatile %struct.ScmObj* %argslist54358$k474782, %struct.ScmObj** %stackaddr$prim56014, align 8
%clofunc56015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47478)
musttail call tailcc void %clofunc56015(%struct.ScmObj* %k47478, %struct.ScmObj* %argslist54358$k474782)
ret void
}

define tailcc void @proc_clo$ae48637(%struct.ScmObj* %env$ae48637,%struct.ScmObj* %current_45args54363) {
%stackaddr$env-ref56016 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48637, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56016
%stackaddr$prim56017 = alloca %struct.ScmObj*, align 8
%k47484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54363)
store volatile %struct.ScmObj* %k47484, %struct.ScmObj** %stackaddr$prim56017, align 8
%stackaddr$prim56018 = alloca %struct.ScmObj*, align 8
%current_45args54364 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54363)
store volatile %struct.ScmObj* %current_45args54364, %struct.ScmObj** %stackaddr$prim56018, align 8
%stackaddr$prim56019 = alloca %struct.ScmObj*, align 8
%f47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54364)
store volatile %struct.ScmObj* %f47123, %struct.ScmObj** %stackaddr$prim56019, align 8
%stackaddr$prim56020 = alloca %struct.ScmObj*, align 8
%current_45args54365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54364)
store volatile %struct.ScmObj* %current_45args54365, %struct.ScmObj** %stackaddr$prim56020, align 8
%stackaddr$prim56021 = alloca %struct.ScmObj*, align 8
%lst47122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54365)
store volatile %struct.ScmObj* %lst47122, %struct.ScmObj** %stackaddr$prim56021, align 8
%stackaddr$makeclosure56022 = alloca %struct.ScmObj*, align 8
%fptrToInt56023 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48638 to i64
%ae48638 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56023)
store volatile %struct.ScmObj* %ae48638, %struct.ScmObj** %stackaddr$makeclosure56022, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48638, %struct.ScmObj* %lst47122, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48638, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48638, %struct.ScmObj* %k47484, i64 2)
%ae48639 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56024 = alloca %struct.ScmObj*, align 8
%fptrToInt56025 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48640 to i64
%ae48640 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56025)
store volatile %struct.ScmObj* %ae48640, %struct.ScmObj** %stackaddr$makeclosure56024, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48640, %struct.ScmObj* %f47123, i64 0)
%argslist54380$ae486380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56026 = alloca %struct.ScmObj*, align 8
%argslist54380$ae486381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48640, %struct.ScmObj* %argslist54380$ae486380)
store volatile %struct.ScmObj* %argslist54380$ae486381, %struct.ScmObj** %stackaddr$prim56026, align 8
%stackaddr$prim56027 = alloca %struct.ScmObj*, align 8
%argslist54380$ae486382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48639, %struct.ScmObj* %argslist54380$ae486381)
store volatile %struct.ScmObj* %argslist54380$ae486382, %struct.ScmObj** %stackaddr$prim56027, align 8
%clofunc56028 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48638)
musttail call tailcc void %clofunc56028(%struct.ScmObj* %ae48638, %struct.ScmObj* %argslist54380$ae486382)
ret void
}

define tailcc void @proc_clo$ae48638(%struct.ScmObj* %env$ae48638,%struct.ScmObj* %current_45args54367) {
%stackaddr$env-ref56029 = alloca %struct.ScmObj*, align 8
%lst47122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48638, i64 0)
store %struct.ScmObj* %lst47122, %struct.ScmObj** %stackaddr$env-ref56029
%stackaddr$env-ref56030 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48638, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56030
%stackaddr$env-ref56031 = alloca %struct.ScmObj*, align 8
%k47484 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48638, i64 2)
store %struct.ScmObj* %k47484, %struct.ScmObj** %stackaddr$env-ref56031
%stackaddr$prim56032 = alloca %struct.ScmObj*, align 8
%_95k47485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54367)
store volatile %struct.ScmObj* %_95k47485, %struct.ScmObj** %stackaddr$prim56032, align 8
%stackaddr$prim56033 = alloca %struct.ScmObj*, align 8
%current_45args54368 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54367)
store volatile %struct.ScmObj* %current_45args54368, %struct.ScmObj** %stackaddr$prim56033, align 8
%stackaddr$prim56034 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54368)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim56034, align 8
%ae48672 = call %struct.ScmObj* @const_init_null()
%argslist54370$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56035 = alloca %struct.ScmObj*, align 8
%argslist54370$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47122, %struct.ScmObj* %argslist54370$_37foldr1470900)
store volatile %struct.ScmObj* %argslist54370$_37foldr1470901, %struct.ScmObj** %stackaddr$prim56035, align 8
%stackaddr$prim56036 = alloca %struct.ScmObj*, align 8
%argslist54370$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48672, %struct.ScmObj* %argslist54370$_37foldr1470901)
store volatile %struct.ScmObj* %argslist54370$_37foldr1470902, %struct.ScmObj** %stackaddr$prim56036, align 8
%stackaddr$prim56037 = alloca %struct.ScmObj*, align 8
%argslist54370$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47256, %struct.ScmObj* %argslist54370$_37foldr1470902)
store volatile %struct.ScmObj* %argslist54370$_37foldr1470903, %struct.ScmObj** %stackaddr$prim56037, align 8
%stackaddr$prim56038 = alloca %struct.ScmObj*, align 8
%argslist54370$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47484, %struct.ScmObj* %argslist54370$_37foldr1470903)
store volatile %struct.ScmObj* %argslist54370$_37foldr1470904, %struct.ScmObj** %stackaddr$prim56038, align 8
%clofunc56039 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc56039(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist54370$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae48640(%struct.ScmObj* %env$ae48640,%struct.ScmObj* %current_45args54371) {
%stackaddr$env-ref56040 = alloca %struct.ScmObj*, align 8
%f47123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48640, i64 0)
store %struct.ScmObj* %f47123, %struct.ScmObj** %stackaddr$env-ref56040
%stackaddr$prim56041 = alloca %struct.ScmObj*, align 8
%k47486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54371)
store volatile %struct.ScmObj* %k47486, %struct.ScmObj** %stackaddr$prim56041, align 8
%stackaddr$prim56042 = alloca %struct.ScmObj*, align 8
%current_45args54372 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54371)
store volatile %struct.ScmObj* %current_45args54372, %struct.ScmObj** %stackaddr$prim56042, align 8
%stackaddr$prim56043 = alloca %struct.ScmObj*, align 8
%v47125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54372)
store volatile %struct.ScmObj* %v47125, %struct.ScmObj** %stackaddr$prim56043, align 8
%stackaddr$prim56044 = alloca %struct.ScmObj*, align 8
%current_45args54373 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54372)
store volatile %struct.ScmObj* %current_45args54373, %struct.ScmObj** %stackaddr$prim56044, align 8
%stackaddr$prim56045 = alloca %struct.ScmObj*, align 8
%r47124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54373)
store volatile %struct.ScmObj* %r47124, %struct.ScmObj** %stackaddr$prim56045, align 8
%stackaddr$makeclosure56046 = alloca %struct.ScmObj*, align 8
%fptrToInt56047 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48642 to i64
%ae48642 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56047)
store volatile %struct.ScmObj* %ae48642, %struct.ScmObj** %stackaddr$makeclosure56046, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48642, %struct.ScmObj* %r47124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48642, %struct.ScmObj* %k47486, i64 1)
%argslist54379$f471230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56048 = alloca %struct.ScmObj*, align 8
%argslist54379$f471231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47125, %struct.ScmObj* %argslist54379$f471230)
store volatile %struct.ScmObj* %argslist54379$f471231, %struct.ScmObj** %stackaddr$prim56048, align 8
%stackaddr$prim56049 = alloca %struct.ScmObj*, align 8
%argslist54379$f471232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48642, %struct.ScmObj* %argslist54379$f471231)
store volatile %struct.ScmObj* %argslist54379$f471232, %struct.ScmObj** %stackaddr$prim56049, align 8
%clofunc56050 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47123)
musttail call tailcc void %clofunc56050(%struct.ScmObj* %f47123, %struct.ScmObj* %argslist54379$f471232)
ret void
}

define tailcc void @proc_clo$ae48642(%struct.ScmObj* %env$ae48642,%struct.ScmObj* %current_45args54375) {
%stackaddr$env-ref56051 = alloca %struct.ScmObj*, align 8
%r47124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48642, i64 0)
store %struct.ScmObj* %r47124, %struct.ScmObj** %stackaddr$env-ref56051
%stackaddr$env-ref56052 = alloca %struct.ScmObj*, align 8
%k47486 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48642, i64 1)
store %struct.ScmObj* %k47486, %struct.ScmObj** %stackaddr$env-ref56052
%stackaddr$prim56053 = alloca %struct.ScmObj*, align 8
%_95k47487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54375)
store volatile %struct.ScmObj* %_95k47487, %struct.ScmObj** %stackaddr$prim56053, align 8
%stackaddr$prim56054 = alloca %struct.ScmObj*, align 8
%current_45args54376 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54375)
store volatile %struct.ScmObj* %current_45args54376, %struct.ScmObj** %stackaddr$prim56054, align 8
%stackaddr$prim56055 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54376)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim56055, align 8
%stackaddr$prim56056 = alloca %struct.ScmObj*, align 8
%cpsprim47488 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47255, %struct.ScmObj* %r47124)
store volatile %struct.ScmObj* %cpsprim47488, %struct.ScmObj** %stackaddr$prim56056, align 8
%ae48647 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54378$k474860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56057 = alloca %struct.ScmObj*, align 8
%argslist54378$k474861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47488, %struct.ScmObj* %argslist54378$k474860)
store volatile %struct.ScmObj* %argslist54378$k474861, %struct.ScmObj** %stackaddr$prim56057, align 8
%stackaddr$prim56058 = alloca %struct.ScmObj*, align 8
%argslist54378$k474862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48647, %struct.ScmObj* %argslist54378$k474861)
store volatile %struct.ScmObj* %argslist54378$k474862, %struct.ScmObj** %stackaddr$prim56058, align 8
%clofunc56059 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47486)
musttail call tailcc void %clofunc56059(%struct.ScmObj* %k47486, %struct.ScmObj* %argslist54378$k474862)
ret void
}

define tailcc void @proc_clo$ae48251(%struct.ScmObj* %env$ae48251,%struct.ScmObj* %current_45args54383) {
%stackaddr$env-ref56060 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48251, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56060
%stackaddr$env-ref56061 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48251, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref56061
%stackaddr$prim56062 = alloca %struct.ScmObj*, align 8
%k47489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54383)
store volatile %struct.ScmObj* %k47489, %struct.ScmObj** %stackaddr$prim56062, align 8
%stackaddr$prim56063 = alloca %struct.ScmObj*, align 8
%current_45args54384 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54383)
store volatile %struct.ScmObj* %current_45args54384, %struct.ScmObj** %stackaddr$prim56063, align 8
%stackaddr$prim56064 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54384)
store volatile %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$prim56064, align 8
%ae48253 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56065 = alloca %struct.ScmObj*, align 8
%fptrToInt56066 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48254 to i64
%ae48254 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56066)
store volatile %struct.ScmObj* %ae48254, %struct.ScmObj** %stackaddr$makeclosure56065, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %_37foldr47096, i64 2)
%argslist54441$k474890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56067 = alloca %struct.ScmObj*, align 8
%argslist54441$k474891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48254, %struct.ScmObj* %argslist54441$k474890)
store volatile %struct.ScmObj* %argslist54441$k474891, %struct.ScmObj** %stackaddr$prim56067, align 8
%stackaddr$prim56068 = alloca %struct.ScmObj*, align 8
%argslist54441$k474892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48253, %struct.ScmObj* %argslist54441$k474891)
store volatile %struct.ScmObj* %argslist54441$k474892, %struct.ScmObj** %stackaddr$prim56068, align 8
%clofunc56069 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47489)
musttail call tailcc void %clofunc56069(%struct.ScmObj* %k47489, %struct.ScmObj* %argslist54441$k474892)
ret void
}

define tailcc void @proc_clo$ae48254(%struct.ScmObj* %env$ae48254,%struct.ScmObj* %args4709747490) {
%stackaddr$env-ref56070 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56070
%stackaddr$env-ref56071 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref56071
%stackaddr$env-ref56072 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 2)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref56072
%stackaddr$prim56073 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4709747490)
store volatile %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$prim56073, align 8
%stackaddr$prim56074 = alloca %struct.ScmObj*, align 8
%args47097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4709747490)
store volatile %struct.ScmObj* %args47097, %struct.ScmObj** %stackaddr$prim56074, align 8
%stackaddr$prim56075 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47097)
store volatile %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$prim56075, align 8
%stackaddr$prim56076 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47097)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim56076, align 8
%stackaddr$prim56077 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47242)
store volatile %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$prim56077, align 8
%stackaddr$prim56078 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47097)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim56078, align 8
%stackaddr$prim56079 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47243)
store volatile %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$prim56079, align 8
%stackaddr$makeclosure56080 = alloca %struct.ScmObj*, align 8
%fptrToInt56081 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48262 to i64
%ae48262 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56081)
store volatile %struct.ScmObj* %ae48262, %struct.ScmObj** %stackaddr$makeclosure56080, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48262, %struct.ScmObj* %k47491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48262, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48262, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48262, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48262, %struct.ScmObj* %acc47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48262, %struct.ScmObj* %lsts47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48262, %struct.ScmObj* %_37foldr47096, i64 6)
%ae48263 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56082 = alloca %struct.ScmObj*, align 8
%fptrToInt56083 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48264 to i64
%ae48264 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56083)
store volatile %struct.ScmObj* %ae48264, %struct.ScmObj** %stackaddr$makeclosure56082, align 8
%argslist54440$ae482620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56084 = alloca %struct.ScmObj*, align 8
%argslist54440$ae482621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48264, %struct.ScmObj* %argslist54440$ae482620)
store volatile %struct.ScmObj* %argslist54440$ae482621, %struct.ScmObj** %stackaddr$prim56084, align 8
%stackaddr$prim56085 = alloca %struct.ScmObj*, align 8
%argslist54440$ae482622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48263, %struct.ScmObj* %argslist54440$ae482621)
store volatile %struct.ScmObj* %argslist54440$ae482622, %struct.ScmObj** %stackaddr$prim56085, align 8
%clofunc56086 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48262)
musttail call tailcc void %clofunc56086(%struct.ScmObj* %ae48262, %struct.ScmObj* %argslist54440$ae482622)
ret void
}

define tailcc void @proc_clo$ae48262(%struct.ScmObj* %env$ae48262,%struct.ScmObj* %current_45args54386) {
%stackaddr$env-ref56087 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48262, i64 0)
store %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$env-ref56087
%stackaddr$env-ref56088 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48262, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56088
%stackaddr$env-ref56089 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48262, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref56089
%stackaddr$env-ref56090 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48262, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref56090
%stackaddr$env-ref56091 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48262, i64 4)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref56091
%stackaddr$env-ref56092 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48262, i64 5)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref56092
%stackaddr$env-ref56093 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48262, i64 6)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref56093
%stackaddr$prim56094 = alloca %struct.ScmObj*, align 8
%_95k47492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54386)
store volatile %struct.ScmObj* %_95k47492, %struct.ScmObj** %stackaddr$prim56094, align 8
%stackaddr$prim56095 = alloca %struct.ScmObj*, align 8
%current_45args54387 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54386)
store volatile %struct.ScmObj* %current_45args54387, %struct.ScmObj** %stackaddr$prim56095, align 8
%stackaddr$prim56096 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54387)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim56096, align 8
%stackaddr$makeclosure56097 = alloca %struct.ScmObj*, align 8
%fptrToInt56098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48294 to i64
%ae48294 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56098)
store volatile %struct.ScmObj* %ae48294, %struct.ScmObj** %stackaddr$makeclosure56097, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %k47491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %acc47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %lsts47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %_37foldr47096, i64 6)
%ae48296 = call %struct.ScmObj* @const_init_false()
%argslist54433$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56099 = alloca %struct.ScmObj*, align 8
%argslist54433$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47098, %struct.ScmObj* %argslist54433$_37foldr1470900)
store volatile %struct.ScmObj* %argslist54433$_37foldr1470901, %struct.ScmObj** %stackaddr$prim56099, align 8
%stackaddr$prim56100 = alloca %struct.ScmObj*, align 8
%argslist54433$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48296, %struct.ScmObj* %argslist54433$_37foldr1470901)
store volatile %struct.ScmObj* %argslist54433$_37foldr1470902, %struct.ScmObj** %stackaddr$prim56100, align 8
%stackaddr$prim56101 = alloca %struct.ScmObj*, align 8
%argslist54433$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47244, %struct.ScmObj* %argslist54433$_37foldr1470902)
store volatile %struct.ScmObj* %argslist54433$_37foldr1470903, %struct.ScmObj** %stackaddr$prim56101, align 8
%stackaddr$prim56102 = alloca %struct.ScmObj*, align 8
%argslist54433$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48294, %struct.ScmObj* %argslist54433$_37foldr1470903)
store volatile %struct.ScmObj* %argslist54433$_37foldr1470904, %struct.ScmObj** %stackaddr$prim56102, align 8
%clofunc56103 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc56103(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist54433$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae48294(%struct.ScmObj* %env$ae48294,%struct.ScmObj* %current_45args54389) {
%stackaddr$env-ref56104 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 0)
store %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$env-ref56104
%stackaddr$env-ref56105 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56105
%stackaddr$env-ref56106 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref56106
%stackaddr$env-ref56107 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref56107
%stackaddr$env-ref56108 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 4)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref56108
%stackaddr$env-ref56109 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 5)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref56109
%stackaddr$env-ref56110 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 6)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref56110
%stackaddr$prim56111 = alloca %struct.ScmObj*, align 8
%_95k47493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54389)
store volatile %struct.ScmObj* %_95k47493, %struct.ScmObj** %stackaddr$prim56111, align 8
%stackaddr$prim56112 = alloca %struct.ScmObj*, align 8
%current_45args54390 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54389)
store volatile %struct.ScmObj* %current_45args54390, %struct.ScmObj** %stackaddr$prim56112, align 8
%stackaddr$prim56113 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54390)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim56113, align 8
%truthy$cmp56114 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47245)
%cmp$cmp56114 = icmp eq i64 %truthy$cmp56114, 1
br i1 %cmp$cmp56114, label %truebranch$cmp56114, label %falsebranch$cmp56114
truebranch$cmp56114:
%ae48305 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54392$k474910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56115 = alloca %struct.ScmObj*, align 8
%argslist54392$k474911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47099, %struct.ScmObj* %argslist54392$k474910)
store volatile %struct.ScmObj* %argslist54392$k474911, %struct.ScmObj** %stackaddr$prim56115, align 8
%stackaddr$prim56116 = alloca %struct.ScmObj*, align 8
%argslist54392$k474912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48305, %struct.ScmObj* %argslist54392$k474911)
store volatile %struct.ScmObj* %argslist54392$k474912, %struct.ScmObj** %stackaddr$prim56116, align 8
%clofunc56117 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47491)
musttail call tailcc void %clofunc56117(%struct.ScmObj* %k47491, %struct.ScmObj* %argslist54392$k474912)
ret void
falsebranch$cmp56114:
%stackaddr$makeclosure56118 = alloca %struct.ScmObj*, align 8
%fptrToInt56119 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48310 to i64
%ae48310 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56119)
store volatile %struct.ScmObj* %ae48310, %struct.ScmObj** %stackaddr$makeclosure56118, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48310, %struct.ScmObj* %k47491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48310, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48310, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48310, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48310, %struct.ScmObj* %acc47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48310, %struct.ScmObj* %lsts47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48310, %struct.ScmObj* %_37foldr47096, i64 6)
%ae48311 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56120 = alloca %struct.ScmObj*, align 8
%fptrToInt56121 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48312 to i64
%ae48312 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56121)
store volatile %struct.ScmObj* %ae48312, %struct.ScmObj** %stackaddr$makeclosure56120, align 8
%argslist54432$ae483100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56122 = alloca %struct.ScmObj*, align 8
%argslist54432$ae483101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48312, %struct.ScmObj* %argslist54432$ae483100)
store volatile %struct.ScmObj* %argslist54432$ae483101, %struct.ScmObj** %stackaddr$prim56122, align 8
%stackaddr$prim56123 = alloca %struct.ScmObj*, align 8
%argslist54432$ae483102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48311, %struct.ScmObj* %argslist54432$ae483101)
store volatile %struct.ScmObj* %argslist54432$ae483102, %struct.ScmObj** %stackaddr$prim56123, align 8
%clofunc56124 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48310)
musttail call tailcc void %clofunc56124(%struct.ScmObj* %ae48310, %struct.ScmObj* %argslist54432$ae483102)
ret void
}

define tailcc void @proc_clo$ae48310(%struct.ScmObj* %env$ae48310,%struct.ScmObj* %current_45args54393) {
%stackaddr$env-ref56125 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48310, i64 0)
store %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$env-ref56125
%stackaddr$env-ref56126 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48310, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56126
%stackaddr$env-ref56127 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48310, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref56127
%stackaddr$env-ref56128 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48310, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref56128
%stackaddr$env-ref56129 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48310, i64 4)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref56129
%stackaddr$env-ref56130 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48310, i64 5)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref56130
%stackaddr$env-ref56131 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48310, i64 6)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref56131
%stackaddr$prim56132 = alloca %struct.ScmObj*, align 8
%_95k47494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54393)
store volatile %struct.ScmObj* %_95k47494, %struct.ScmObj** %stackaddr$prim56132, align 8
%stackaddr$prim56133 = alloca %struct.ScmObj*, align 8
%current_45args54394 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54393)
store volatile %struct.ScmObj* %current_45args54394, %struct.ScmObj** %stackaddr$prim56133, align 8
%stackaddr$prim56134 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54394)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim56134, align 8
%stackaddr$makeclosure56135 = alloca %struct.ScmObj*, align 8
%fptrToInt56136 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48331 to i64
%ae48331 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56136)
store volatile %struct.ScmObj* %ae48331, %struct.ScmObj** %stackaddr$makeclosure56135, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %k47491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %acc47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %lsts47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %_37foldr47096, i64 6)
%argslist54427$_37map1470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56137 = alloca %struct.ScmObj*, align 8
%argslist54427$_37map1470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47098, %struct.ScmObj* %argslist54427$_37map1470860)
store volatile %struct.ScmObj* %argslist54427$_37map1470861, %struct.ScmObj** %stackaddr$prim56137, align 8
%stackaddr$prim56138 = alloca %struct.ScmObj*, align 8
%argslist54427$_37map1470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47246, %struct.ScmObj* %argslist54427$_37map1470861)
store volatile %struct.ScmObj* %argslist54427$_37map1470862, %struct.ScmObj** %stackaddr$prim56138, align 8
%stackaddr$prim56139 = alloca %struct.ScmObj*, align 8
%argslist54427$_37map1470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48331, %struct.ScmObj* %argslist54427$_37map1470862)
store volatile %struct.ScmObj* %argslist54427$_37map1470863, %struct.ScmObj** %stackaddr$prim56139, align 8
%clofunc56140 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147086)
musttail call tailcc void %clofunc56140(%struct.ScmObj* %_37map147086, %struct.ScmObj* %argslist54427$_37map1470863)
ret void
}

define tailcc void @proc_clo$ae48331(%struct.ScmObj* %env$ae48331,%struct.ScmObj* %current_45args54396) {
%stackaddr$env-ref56141 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 0)
store %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$env-ref56141
%stackaddr$env-ref56142 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56142
%stackaddr$env-ref56143 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref56143
%stackaddr$env-ref56144 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref56144
%stackaddr$env-ref56145 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 4)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref56145
%stackaddr$env-ref56146 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 5)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref56146
%stackaddr$env-ref56147 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 6)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref56147
%stackaddr$prim56148 = alloca %struct.ScmObj*, align 8
%_95k47495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54396)
store volatile %struct.ScmObj* %_95k47495, %struct.ScmObj** %stackaddr$prim56148, align 8
%stackaddr$prim56149 = alloca %struct.ScmObj*, align 8
%current_45args54397 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54396)
store volatile %struct.ScmObj* %current_45args54397, %struct.ScmObj** %stackaddr$prim56149, align 8
%stackaddr$prim56150 = alloca %struct.ScmObj*, align 8
%lsts_4347105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54397)
store volatile %struct.ScmObj* %lsts_4347105, %struct.ScmObj** %stackaddr$prim56150, align 8
%stackaddr$makeclosure56151 = alloca %struct.ScmObj*, align 8
%fptrToInt56152 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48334 to i64
%ae48334 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56152)
store volatile %struct.ScmObj* %ae48334, %struct.ScmObj** %stackaddr$makeclosure56151, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48334, %struct.ScmObj* %k47491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48334, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48334, %struct.ScmObj* %lsts_4347105, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48334, %struct.ScmObj* %_37map147086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48334, %struct.ScmObj* %f47100, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48334, %struct.ScmObj* %acc47099, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48334, %struct.ScmObj* %lsts47098, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48334, %struct.ScmObj* %_37foldr47096, i64 7)
%ae48335 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56153 = alloca %struct.ScmObj*, align 8
%fptrToInt56154 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48336 to i64
%ae48336 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56154)
store volatile %struct.ScmObj* %ae48336, %struct.ScmObj** %stackaddr$makeclosure56153, align 8
%argslist54426$ae483340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56155 = alloca %struct.ScmObj*, align 8
%argslist54426$ae483341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48336, %struct.ScmObj* %argslist54426$ae483340)
store volatile %struct.ScmObj* %argslist54426$ae483341, %struct.ScmObj** %stackaddr$prim56155, align 8
%stackaddr$prim56156 = alloca %struct.ScmObj*, align 8
%argslist54426$ae483342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48335, %struct.ScmObj* %argslist54426$ae483341)
store volatile %struct.ScmObj* %argslist54426$ae483342, %struct.ScmObj** %stackaddr$prim56156, align 8
%clofunc56157 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48334)
musttail call tailcc void %clofunc56157(%struct.ScmObj* %ae48334, %struct.ScmObj* %argslist54426$ae483342)
ret void
}

define tailcc void @proc_clo$ae48334(%struct.ScmObj* %env$ae48334,%struct.ScmObj* %current_45args54399) {
%stackaddr$env-ref56158 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48334, i64 0)
store %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$env-ref56158
%stackaddr$env-ref56159 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48334, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56159
%stackaddr$env-ref56160 = alloca %struct.ScmObj*, align 8
%lsts_4347105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48334, i64 2)
store %struct.ScmObj* %lsts_4347105, %struct.ScmObj** %stackaddr$env-ref56160
%stackaddr$env-ref56161 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48334, i64 3)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref56161
%stackaddr$env-ref56162 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48334, i64 4)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref56162
%stackaddr$env-ref56163 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48334, i64 5)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref56163
%stackaddr$env-ref56164 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48334, i64 6)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref56164
%stackaddr$env-ref56165 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48334, i64 7)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref56165
%stackaddr$prim56166 = alloca %struct.ScmObj*, align 8
%_95k47496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54399)
store volatile %struct.ScmObj* %_95k47496, %struct.ScmObj** %stackaddr$prim56166, align 8
%stackaddr$prim56167 = alloca %struct.ScmObj*, align 8
%current_45args54400 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54399)
store volatile %struct.ScmObj* %current_45args54400, %struct.ScmObj** %stackaddr$prim56167, align 8
%stackaddr$prim56168 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54400)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim56168, align 8
%stackaddr$makeclosure56169 = alloca %struct.ScmObj*, align 8
%fptrToInt56170 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48355 to i64
%ae48355 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56170)
store volatile %struct.ScmObj* %ae48355, %struct.ScmObj** %stackaddr$makeclosure56169, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48355, %struct.ScmObj* %k47491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48355, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48355, %struct.ScmObj* %lsts_4347105, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48355, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48355, %struct.ScmObj* %acc47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48355, %struct.ScmObj* %_37foldr47096, i64 5)
%argslist54421$_37map1470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56171 = alloca %struct.ScmObj*, align 8
%argslist54421$_37map1470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47098, %struct.ScmObj* %argslist54421$_37map1470860)
store volatile %struct.ScmObj* %argslist54421$_37map1470861, %struct.ScmObj** %stackaddr$prim56171, align 8
%stackaddr$prim56172 = alloca %struct.ScmObj*, align 8
%argslist54421$_37map1470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47247, %struct.ScmObj* %argslist54421$_37map1470861)
store volatile %struct.ScmObj* %argslist54421$_37map1470862, %struct.ScmObj** %stackaddr$prim56172, align 8
%stackaddr$prim56173 = alloca %struct.ScmObj*, align 8
%argslist54421$_37map1470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48355, %struct.ScmObj* %argslist54421$_37map1470862)
store volatile %struct.ScmObj* %argslist54421$_37map1470863, %struct.ScmObj** %stackaddr$prim56173, align 8
%clofunc56174 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147086)
musttail call tailcc void %clofunc56174(%struct.ScmObj* %_37map147086, %struct.ScmObj* %argslist54421$_37map1470863)
ret void
}

define tailcc void @proc_clo$ae48355(%struct.ScmObj* %env$ae48355,%struct.ScmObj* %current_45args54402) {
%stackaddr$env-ref56175 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48355, i64 0)
store %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$env-ref56175
%stackaddr$env-ref56176 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48355, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56176
%stackaddr$env-ref56177 = alloca %struct.ScmObj*, align 8
%lsts_4347105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48355, i64 2)
store %struct.ScmObj* %lsts_4347105, %struct.ScmObj** %stackaddr$env-ref56177
%stackaddr$env-ref56178 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48355, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref56178
%stackaddr$env-ref56179 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48355, i64 4)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref56179
%stackaddr$env-ref56180 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48355, i64 5)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref56180
%stackaddr$prim56181 = alloca %struct.ScmObj*, align 8
%_95k47497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54402)
store volatile %struct.ScmObj* %_95k47497, %struct.ScmObj** %stackaddr$prim56181, align 8
%stackaddr$prim56182 = alloca %struct.ScmObj*, align 8
%current_45args54403 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54402)
store volatile %struct.ScmObj* %current_45args54403, %struct.ScmObj** %stackaddr$prim56182, align 8
%stackaddr$prim56183 = alloca %struct.ScmObj*, align 8
%vs47103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54403)
store volatile %struct.ScmObj* %vs47103, %struct.ScmObj** %stackaddr$prim56183, align 8
%stackaddr$makeclosure56184 = alloca %struct.ScmObj*, align 8
%fptrToInt56185 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48358 to i64
%ae48358 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56185)
store volatile %struct.ScmObj* %ae48358, %struct.ScmObj** %stackaddr$makeclosure56184, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48358, %struct.ScmObj* %k47491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48358, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48358, %struct.ScmObj* %lsts_4347105, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48358, %struct.ScmObj* %vs47103, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48358, %struct.ScmObj* %f47100, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48358, %struct.ScmObj* %acc47099, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48358, %struct.ScmObj* %_37foldr47096, i64 6)
%ae48359 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56186 = alloca %struct.ScmObj*, align 8
%fptrToInt56187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48360 to i64
%ae48360 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56187)
store volatile %struct.ScmObj* %ae48360, %struct.ScmObj** %stackaddr$makeclosure56186, align 8
%argslist54420$ae483580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56188 = alloca %struct.ScmObj*, align 8
%argslist54420$ae483581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48360, %struct.ScmObj* %argslist54420$ae483580)
store volatile %struct.ScmObj* %argslist54420$ae483581, %struct.ScmObj** %stackaddr$prim56188, align 8
%stackaddr$prim56189 = alloca %struct.ScmObj*, align 8
%argslist54420$ae483582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48359, %struct.ScmObj* %argslist54420$ae483581)
store volatile %struct.ScmObj* %argslist54420$ae483582, %struct.ScmObj** %stackaddr$prim56189, align 8
%clofunc56190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48358)
musttail call tailcc void %clofunc56190(%struct.ScmObj* %ae48358, %struct.ScmObj* %argslist54420$ae483582)
ret void
}

define tailcc void @proc_clo$ae48358(%struct.ScmObj* %env$ae48358,%struct.ScmObj* %current_45args54405) {
%stackaddr$env-ref56191 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48358, i64 0)
store %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$env-ref56191
%stackaddr$env-ref56192 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48358, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56192
%stackaddr$env-ref56193 = alloca %struct.ScmObj*, align 8
%lsts_4347105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48358, i64 2)
store %struct.ScmObj* %lsts_4347105, %struct.ScmObj** %stackaddr$env-ref56193
%stackaddr$env-ref56194 = alloca %struct.ScmObj*, align 8
%vs47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48358, i64 3)
store %struct.ScmObj* %vs47103, %struct.ScmObj** %stackaddr$env-ref56194
%stackaddr$env-ref56195 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48358, i64 4)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref56195
%stackaddr$env-ref56196 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48358, i64 5)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref56196
%stackaddr$env-ref56197 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48358, i64 6)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref56197
%stackaddr$prim56198 = alloca %struct.ScmObj*, align 8
%_95k47498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54405)
store volatile %struct.ScmObj* %_95k47498, %struct.ScmObj** %stackaddr$prim56198, align 8
%stackaddr$prim56199 = alloca %struct.ScmObj*, align 8
%current_45args54406 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54405)
store volatile %struct.ScmObj* %current_45args54406, %struct.ScmObj** %stackaddr$prim56199, align 8
%stackaddr$prim56200 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54406)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim56200, align 8
%stackaddr$prim56201 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47099, %struct.ScmObj* %lsts_4347105)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim56201, align 8
%stackaddr$prim56202 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47100, %struct.ScmObj* %anf_45bind47249)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim56202, align 8
%stackaddr$makeclosure56203 = alloca %struct.ScmObj*, align 8
%fptrToInt56204 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48384 to i64
%ae48384 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56204)
store volatile %struct.ScmObj* %ae48384, %struct.ScmObj** %stackaddr$makeclosure56203, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48384, %struct.ScmObj* %k47491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48384, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48384, %struct.ScmObj* %anf_45bind47248, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48384, %struct.ScmObj* %vs47103, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48384, %struct.ScmObj* %f47100, i64 4)
%stackaddr$prim56205 = alloca %struct.ScmObj*, align 8
%cpsargs47502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48384, %struct.ScmObj* %anf_45bind47250)
store volatile %struct.ScmObj* %cpsargs47502, %struct.ScmObj** %stackaddr$prim56205, align 8
%clofunc56206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47096)
musttail call tailcc void %clofunc56206(%struct.ScmObj* %_37foldr47096, %struct.ScmObj* %cpsargs47502)
ret void
}

define tailcc void @proc_clo$ae48384(%struct.ScmObj* %env$ae48384,%struct.ScmObj* %current_45args54408) {
%stackaddr$env-ref56207 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48384, i64 0)
store %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$env-ref56207
%stackaddr$env-ref56208 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48384, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56208
%stackaddr$env-ref56209 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48384, i64 2)
store %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$env-ref56209
%stackaddr$env-ref56210 = alloca %struct.ScmObj*, align 8
%vs47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48384, i64 3)
store %struct.ScmObj* %vs47103, %struct.ScmObj** %stackaddr$env-ref56210
%stackaddr$env-ref56211 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48384, i64 4)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref56211
%stackaddr$prim56212 = alloca %struct.ScmObj*, align 8
%_95k47499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54408)
store volatile %struct.ScmObj* %_95k47499, %struct.ScmObj** %stackaddr$prim56212, align 8
%stackaddr$prim56213 = alloca %struct.ScmObj*, align 8
%current_45args54409 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54408)
store volatile %struct.ScmObj* %current_45args54409, %struct.ScmObj** %stackaddr$prim56213, align 8
%stackaddr$prim56214 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54409)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim56214, align 8
%ae48389 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56215 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47251, %struct.ScmObj* %ae48389)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim56215, align 8
%stackaddr$makeclosure56216 = alloca %struct.ScmObj*, align 8
%fptrToInt56217 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48391 to i64
%ae48391 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56217)
store volatile %struct.ScmObj* %ae48391, %struct.ScmObj** %stackaddr$makeclosure56216, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48391, %struct.ScmObj* %k47491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48391, %struct.ScmObj* %f47100, i64 1)
%argslist54414$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56218 = alloca %struct.ScmObj*, align 8
%argslist54414$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47103, %struct.ScmObj* %argslist54414$_37foldr1470900)
store volatile %struct.ScmObj* %argslist54414$_37foldr1470901, %struct.ScmObj** %stackaddr$prim56218, align 8
%stackaddr$prim56219 = alloca %struct.ScmObj*, align 8
%argslist54414$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47252, %struct.ScmObj* %argslist54414$_37foldr1470901)
store volatile %struct.ScmObj* %argslist54414$_37foldr1470902, %struct.ScmObj** %stackaddr$prim56219, align 8
%stackaddr$prim56220 = alloca %struct.ScmObj*, align 8
%argslist54414$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47248, %struct.ScmObj* %argslist54414$_37foldr1470902)
store volatile %struct.ScmObj* %argslist54414$_37foldr1470903, %struct.ScmObj** %stackaddr$prim56220, align 8
%stackaddr$prim56221 = alloca %struct.ScmObj*, align 8
%argslist54414$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48391, %struct.ScmObj* %argslist54414$_37foldr1470903)
store volatile %struct.ScmObj* %argslist54414$_37foldr1470904, %struct.ScmObj** %stackaddr$prim56221, align 8
%clofunc56222 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc56222(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist54414$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae48391(%struct.ScmObj* %env$ae48391,%struct.ScmObj* %current_45args54411) {
%stackaddr$env-ref56223 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48391, i64 0)
store %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$env-ref56223
%stackaddr$env-ref56224 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48391, i64 1)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref56224
%stackaddr$prim56225 = alloca %struct.ScmObj*, align 8
%_95k47500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54411)
store volatile %struct.ScmObj* %_95k47500, %struct.ScmObj** %stackaddr$prim56225, align 8
%stackaddr$prim56226 = alloca %struct.ScmObj*, align 8
%current_45args54412 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54411)
store volatile %struct.ScmObj* %current_45args54412, %struct.ScmObj** %stackaddr$prim56226, align 8
%stackaddr$prim56227 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54412)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim56227, align 8
%stackaddr$prim56228 = alloca %struct.ScmObj*, align 8
%cpsargs47501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47491, %struct.ScmObj* %anf_45bind47253)
store volatile %struct.ScmObj* %cpsargs47501, %struct.ScmObj** %stackaddr$prim56228, align 8
%clofunc56229 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47100)
musttail call tailcc void %clofunc56229(%struct.ScmObj* %f47100, %struct.ScmObj* %cpsargs47501)
ret void
}

define tailcc void @proc_clo$ae48360(%struct.ScmObj* %env$ae48360,%struct.ScmObj* %current_45args54415) {
%stackaddr$prim56230 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54415)
store volatile %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$prim56230, align 8
%stackaddr$prim56231 = alloca %struct.ScmObj*, align 8
%current_45args54416 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54415)
store volatile %struct.ScmObj* %current_45args54416, %struct.ScmObj** %stackaddr$prim56231, align 8
%stackaddr$prim56232 = alloca %struct.ScmObj*, align 8
%a47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54416)
store volatile %struct.ScmObj* %a47108, %struct.ScmObj** %stackaddr$prim56232, align 8
%stackaddr$prim56233 = alloca %struct.ScmObj*, align 8
%current_45args54417 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54416)
store volatile %struct.ScmObj* %current_45args54417, %struct.ScmObj** %stackaddr$prim56233, align 8
%stackaddr$prim56234 = alloca %struct.ScmObj*, align 8
%b47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54417)
store volatile %struct.ScmObj* %b47107, %struct.ScmObj** %stackaddr$prim56234, align 8
%stackaddr$prim56235 = alloca %struct.ScmObj*, align 8
%cpsprim47504 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47108, %struct.ScmObj* %b47107)
store volatile %struct.ScmObj* %cpsprim47504, %struct.ScmObj** %stackaddr$prim56235, align 8
%ae48364 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54419$k475030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56236 = alloca %struct.ScmObj*, align 8
%argslist54419$k475031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47504, %struct.ScmObj* %argslist54419$k475030)
store volatile %struct.ScmObj* %argslist54419$k475031, %struct.ScmObj** %stackaddr$prim56236, align 8
%stackaddr$prim56237 = alloca %struct.ScmObj*, align 8
%argslist54419$k475032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48364, %struct.ScmObj* %argslist54419$k475031)
store volatile %struct.ScmObj* %argslist54419$k475032, %struct.ScmObj** %stackaddr$prim56237, align 8
%clofunc56238 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47503)
musttail call tailcc void %clofunc56238(%struct.ScmObj* %k47503, %struct.ScmObj* %argslist54419$k475032)
ret void
}

define tailcc void @proc_clo$ae48336(%struct.ScmObj* %env$ae48336,%struct.ScmObj* %current_45args54422) {
%stackaddr$prim56239 = alloca %struct.ScmObj*, align 8
%k47505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54422)
store volatile %struct.ScmObj* %k47505, %struct.ScmObj** %stackaddr$prim56239, align 8
%stackaddr$prim56240 = alloca %struct.ScmObj*, align 8
%current_45args54423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54422)
store volatile %struct.ScmObj* %current_45args54423, %struct.ScmObj** %stackaddr$prim56240, align 8
%stackaddr$prim56241 = alloca %struct.ScmObj*, align 8
%x47104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54423)
store volatile %struct.ScmObj* %x47104, %struct.ScmObj** %stackaddr$prim56241, align 8
%stackaddr$prim56242 = alloca %struct.ScmObj*, align 8
%cpsprim47506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47104)
store volatile %struct.ScmObj* %cpsprim47506, %struct.ScmObj** %stackaddr$prim56242, align 8
%ae48339 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54425$k475050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56243 = alloca %struct.ScmObj*, align 8
%argslist54425$k475051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47506, %struct.ScmObj* %argslist54425$k475050)
store volatile %struct.ScmObj* %argslist54425$k475051, %struct.ScmObj** %stackaddr$prim56243, align 8
%stackaddr$prim56244 = alloca %struct.ScmObj*, align 8
%argslist54425$k475052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48339, %struct.ScmObj* %argslist54425$k475051)
store volatile %struct.ScmObj* %argslist54425$k475052, %struct.ScmObj** %stackaddr$prim56244, align 8
%clofunc56245 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47505)
musttail call tailcc void %clofunc56245(%struct.ScmObj* %k47505, %struct.ScmObj* %argslist54425$k475052)
ret void
}

define tailcc void @proc_clo$ae48312(%struct.ScmObj* %env$ae48312,%struct.ScmObj* %current_45args54428) {
%stackaddr$prim56246 = alloca %struct.ScmObj*, align 8
%k47507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54428)
store volatile %struct.ScmObj* %k47507, %struct.ScmObj** %stackaddr$prim56246, align 8
%stackaddr$prim56247 = alloca %struct.ScmObj*, align 8
%current_45args54429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54428)
store volatile %struct.ScmObj* %current_45args54429, %struct.ScmObj** %stackaddr$prim56247, align 8
%stackaddr$prim56248 = alloca %struct.ScmObj*, align 8
%x47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54429)
store volatile %struct.ScmObj* %x47106, %struct.ScmObj** %stackaddr$prim56248, align 8
%stackaddr$prim56249 = alloca %struct.ScmObj*, align 8
%cpsprim47508 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47106)
store volatile %struct.ScmObj* %cpsprim47508, %struct.ScmObj** %stackaddr$prim56249, align 8
%ae48315 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54431$k475070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56250 = alloca %struct.ScmObj*, align 8
%argslist54431$k475071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47508, %struct.ScmObj* %argslist54431$k475070)
store volatile %struct.ScmObj* %argslist54431$k475071, %struct.ScmObj** %stackaddr$prim56250, align 8
%stackaddr$prim56251 = alloca %struct.ScmObj*, align 8
%argslist54431$k475072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48315, %struct.ScmObj* %argslist54431$k475071)
store volatile %struct.ScmObj* %argslist54431$k475072, %struct.ScmObj** %stackaddr$prim56251, align 8
%clofunc56252 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47507)
musttail call tailcc void %clofunc56252(%struct.ScmObj* %k47507, %struct.ScmObj* %argslist54431$k475072)
ret void
}

define tailcc void @proc_clo$ae48264(%struct.ScmObj* %env$ae48264,%struct.ScmObj* %current_45args54434) {
%stackaddr$prim56253 = alloca %struct.ScmObj*, align 8
%k47509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54434)
store volatile %struct.ScmObj* %k47509, %struct.ScmObj** %stackaddr$prim56253, align 8
%stackaddr$prim56254 = alloca %struct.ScmObj*, align 8
%current_45args54435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54434)
store volatile %struct.ScmObj* %current_45args54435, %struct.ScmObj** %stackaddr$prim56254, align 8
%stackaddr$prim56255 = alloca %struct.ScmObj*, align 8
%lst47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54435)
store volatile %struct.ScmObj* %lst47102, %struct.ScmObj** %stackaddr$prim56255, align 8
%stackaddr$prim56256 = alloca %struct.ScmObj*, align 8
%current_45args54436 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54435)
store volatile %struct.ScmObj* %current_45args54436, %struct.ScmObj** %stackaddr$prim56256, align 8
%stackaddr$prim56257 = alloca %struct.ScmObj*, align 8
%b47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54436)
store volatile %struct.ScmObj* %b47101, %struct.ScmObj** %stackaddr$prim56257, align 8
%truthy$cmp56258 = call i64 @is_truthy_value(%struct.ScmObj* %b47101)
%cmp$cmp56258 = icmp eq i64 %truthy$cmp56258, 1
br i1 %cmp$cmp56258, label %truebranch$cmp56258, label %falsebranch$cmp56258
truebranch$cmp56258:
%ae48267 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54438$k475090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56259 = alloca %struct.ScmObj*, align 8
%argslist54438$k475091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47101, %struct.ScmObj* %argslist54438$k475090)
store volatile %struct.ScmObj* %argslist54438$k475091, %struct.ScmObj** %stackaddr$prim56259, align 8
%stackaddr$prim56260 = alloca %struct.ScmObj*, align 8
%argslist54438$k475092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48267, %struct.ScmObj* %argslist54438$k475091)
store volatile %struct.ScmObj* %argslist54438$k475092, %struct.ScmObj** %stackaddr$prim56260, align 8
%clofunc56261 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47509)
musttail call tailcc void %clofunc56261(%struct.ScmObj* %k47509, %struct.ScmObj* %argslist54438$k475092)
ret void
falsebranch$cmp56258:
%stackaddr$prim56262 = alloca %struct.ScmObj*, align 8
%cpsprim47510 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47102)
store volatile %struct.ScmObj* %cpsprim47510, %struct.ScmObj** %stackaddr$prim56262, align 8
%ae48274 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54439$k475090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56263 = alloca %struct.ScmObj*, align 8
%argslist54439$k475091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47510, %struct.ScmObj* %argslist54439$k475090)
store volatile %struct.ScmObj* %argslist54439$k475091, %struct.ScmObj** %stackaddr$prim56263, align 8
%stackaddr$prim56264 = alloca %struct.ScmObj*, align 8
%argslist54439$k475092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48274, %struct.ScmObj* %argslist54439$k475091)
store volatile %struct.ScmObj* %argslist54439$k475092, %struct.ScmObj** %stackaddr$prim56264, align 8
%clofunc56265 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47509)
musttail call tailcc void %clofunc56265(%struct.ScmObj* %k47509, %struct.ScmObj* %argslist54439$k475092)
ret void
}

define tailcc void @proc_clo$ae48221(%struct.ScmObj* %env$ae48221,%struct.ScmObj* %current_45args54443) {
%stackaddr$env-ref56266 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48221, i64 0)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref56266
%stackaddr$env-ref56267 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48221, i64 1)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref56267
%stackaddr$prim56268 = alloca %struct.ScmObj*, align 8
%k47511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54443)
store volatile %struct.ScmObj* %k47511, %struct.ScmObj** %stackaddr$prim56268, align 8
%stackaddr$prim56269 = alloca %struct.ScmObj*, align 8
%current_45args54444 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54443)
store volatile %struct.ScmObj* %current_45args54444, %struct.ScmObj** %stackaddr$prim56269, align 8
%stackaddr$prim56270 = alloca %struct.ScmObj*, align 8
%lst47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54444)
store volatile %struct.ScmObj* %lst47111, %struct.ScmObj** %stackaddr$prim56270, align 8
%stackaddr$prim56271 = alloca %struct.ScmObj*, align 8
%current_45args54445 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54444)
store volatile %struct.ScmObj* %current_45args54445, %struct.ScmObj** %stackaddr$prim56271, align 8
%stackaddr$prim56272 = alloca %struct.ScmObj*, align 8
%n47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54445)
store volatile %struct.ScmObj* %n47110, %struct.ScmObj** %stackaddr$prim56272, align 8
%stackaddr$makeclosure56273 = alloca %struct.ScmObj*, align 8
%fptrToInt56274 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48223 to i64
%ae48223 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56274)
store volatile %struct.ScmObj* %ae48223, %struct.ScmObj** %stackaddr$makeclosure56273, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48223, %struct.ScmObj* %lst47111, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48223, %struct.ScmObj* %k47511, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48223, %struct.ScmObj* %n47110, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48223, %struct.ScmObj* %_37take47082, i64 3)
%argslist54451$_37length470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56275 = alloca %struct.ScmObj*, align 8
%argslist54451$_37length470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47111, %struct.ScmObj* %argslist54451$_37length470790)
store volatile %struct.ScmObj* %argslist54451$_37length470791, %struct.ScmObj** %stackaddr$prim56275, align 8
%stackaddr$prim56276 = alloca %struct.ScmObj*, align 8
%argslist54451$_37length470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48223, %struct.ScmObj* %argslist54451$_37length470791)
store volatile %struct.ScmObj* %argslist54451$_37length470792, %struct.ScmObj** %stackaddr$prim56276, align 8
%clofunc56277 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47079)
musttail call tailcc void %clofunc56277(%struct.ScmObj* %_37length47079, %struct.ScmObj* %argslist54451$_37length470792)
ret void
}

define tailcc void @proc_clo$ae48223(%struct.ScmObj* %env$ae48223,%struct.ScmObj* %current_45args54447) {
%stackaddr$env-ref56278 = alloca %struct.ScmObj*, align 8
%lst47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48223, i64 0)
store %struct.ScmObj* %lst47111, %struct.ScmObj** %stackaddr$env-ref56278
%stackaddr$env-ref56279 = alloca %struct.ScmObj*, align 8
%k47511 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48223, i64 1)
store %struct.ScmObj* %k47511, %struct.ScmObj** %stackaddr$env-ref56279
%stackaddr$env-ref56280 = alloca %struct.ScmObj*, align 8
%n47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48223, i64 2)
store %struct.ScmObj* %n47110, %struct.ScmObj** %stackaddr$env-ref56280
%stackaddr$env-ref56281 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48223, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref56281
%stackaddr$prim56282 = alloca %struct.ScmObj*, align 8
%_95k47512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54447)
store volatile %struct.ScmObj* %_95k47512, %struct.ScmObj** %stackaddr$prim56282, align 8
%stackaddr$prim56283 = alloca %struct.ScmObj*, align 8
%current_45args54448 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54447)
store volatile %struct.ScmObj* %current_45args54448, %struct.ScmObj** %stackaddr$prim56283, align 8
%stackaddr$prim56284 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54448)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim56284, align 8
%stackaddr$prim56285 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47240, %struct.ScmObj* %n47110)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim56285, align 8
%argslist54450$_37take470820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56286 = alloca %struct.ScmObj*, align 8
%argslist54450$_37take470821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47241, %struct.ScmObj* %argslist54450$_37take470820)
store volatile %struct.ScmObj* %argslist54450$_37take470821, %struct.ScmObj** %stackaddr$prim56286, align 8
%stackaddr$prim56287 = alloca %struct.ScmObj*, align 8
%argslist54450$_37take470822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47111, %struct.ScmObj* %argslist54450$_37take470821)
store volatile %struct.ScmObj* %argslist54450$_37take470822, %struct.ScmObj** %stackaddr$prim56287, align 8
%stackaddr$prim56288 = alloca %struct.ScmObj*, align 8
%argslist54450$_37take470823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47511, %struct.ScmObj* %argslist54450$_37take470822)
store volatile %struct.ScmObj* %argslist54450$_37take470823, %struct.ScmObj** %stackaddr$prim56288, align 8
%clofunc56289 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47082)
musttail call tailcc void %clofunc56289(%struct.ScmObj* %_37take47082, %struct.ScmObj* %argslist54450$_37take470823)
ret void
}

define tailcc void @proc_clo$ae48167(%struct.ScmObj* %env$ae48167,%struct.ScmObj* %current_45args54453) {
%stackaddr$env-ref56290 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48167, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref56290
%stackaddr$prim56291 = alloca %struct.ScmObj*, align 8
%k47513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54453)
store volatile %struct.ScmObj* %k47513, %struct.ScmObj** %stackaddr$prim56291, align 8
%stackaddr$prim56292 = alloca %struct.ScmObj*, align 8
%current_45args54454 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54453)
store volatile %struct.ScmObj* %current_45args54454, %struct.ScmObj** %stackaddr$prim56292, align 8
%stackaddr$prim56293 = alloca %struct.ScmObj*, align 8
%lst47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54454)
store volatile %struct.ScmObj* %lst47113, %struct.ScmObj** %stackaddr$prim56293, align 8
%stackaddr$makeclosure56294 = alloca %struct.ScmObj*, align 8
%fptrToInt56295 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48168 to i64
%ae48168 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56295)
store volatile %struct.ScmObj* %ae48168, %struct.ScmObj** %stackaddr$makeclosure56294, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48168, %struct.ScmObj* %lst47113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48168, %struct.ScmObj* %k47513, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48168, %struct.ScmObj* %_37foldl147074, i64 2)
%ae48169 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56296 = alloca %struct.ScmObj*, align 8
%fptrToInt56297 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48170 to i64
%ae48170 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56297)
store volatile %struct.ScmObj* %ae48170, %struct.ScmObj** %stackaddr$makeclosure56296, align 8
%argslist54465$ae481680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56298 = alloca %struct.ScmObj*, align 8
%argslist54465$ae481681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48170, %struct.ScmObj* %argslist54465$ae481680)
store volatile %struct.ScmObj* %argslist54465$ae481681, %struct.ScmObj** %stackaddr$prim56298, align 8
%stackaddr$prim56299 = alloca %struct.ScmObj*, align 8
%argslist54465$ae481682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48169, %struct.ScmObj* %argslist54465$ae481681)
store volatile %struct.ScmObj* %argslist54465$ae481682, %struct.ScmObj** %stackaddr$prim56299, align 8
%clofunc56300 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48168)
musttail call tailcc void %clofunc56300(%struct.ScmObj* %ae48168, %struct.ScmObj* %argslist54465$ae481682)
ret void
}

define tailcc void @proc_clo$ae48168(%struct.ScmObj* %env$ae48168,%struct.ScmObj* %current_45args54456) {
%stackaddr$env-ref56301 = alloca %struct.ScmObj*, align 8
%lst47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48168, i64 0)
store %struct.ScmObj* %lst47113, %struct.ScmObj** %stackaddr$env-ref56301
%stackaddr$env-ref56302 = alloca %struct.ScmObj*, align 8
%k47513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48168, i64 1)
store %struct.ScmObj* %k47513, %struct.ScmObj** %stackaddr$env-ref56302
%stackaddr$env-ref56303 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48168, i64 2)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref56303
%stackaddr$prim56304 = alloca %struct.ScmObj*, align 8
%_95k47514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54456)
store volatile %struct.ScmObj* %_95k47514, %struct.ScmObj** %stackaddr$prim56304, align 8
%stackaddr$prim56305 = alloca %struct.ScmObj*, align 8
%current_45args54457 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54456)
store volatile %struct.ScmObj* %current_45args54457, %struct.ScmObj** %stackaddr$prim56305, align 8
%stackaddr$prim56306 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54457)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim56306, align 8
%ae48189 = call %struct.ScmObj* @const_init_null()
%argslist54459$_37foldl1470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56307 = alloca %struct.ScmObj*, align 8
%argslist54459$_37foldl1470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47113, %struct.ScmObj* %argslist54459$_37foldl1470740)
store volatile %struct.ScmObj* %argslist54459$_37foldl1470741, %struct.ScmObj** %stackaddr$prim56307, align 8
%stackaddr$prim56308 = alloca %struct.ScmObj*, align 8
%argslist54459$_37foldl1470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48189, %struct.ScmObj* %argslist54459$_37foldl1470741)
store volatile %struct.ScmObj* %argslist54459$_37foldl1470742, %struct.ScmObj** %stackaddr$prim56308, align 8
%stackaddr$prim56309 = alloca %struct.ScmObj*, align 8
%argslist54459$_37foldl1470743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47239, %struct.ScmObj* %argslist54459$_37foldl1470742)
store volatile %struct.ScmObj* %argslist54459$_37foldl1470743, %struct.ScmObj** %stackaddr$prim56309, align 8
%stackaddr$prim56310 = alloca %struct.ScmObj*, align 8
%argslist54459$_37foldl1470744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47513, %struct.ScmObj* %argslist54459$_37foldl1470743)
store volatile %struct.ScmObj* %argslist54459$_37foldl1470744, %struct.ScmObj** %stackaddr$prim56310, align 8
%clofunc56311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147074)
musttail call tailcc void %clofunc56311(%struct.ScmObj* %_37foldl147074, %struct.ScmObj* %argslist54459$_37foldl1470744)
ret void
}

define tailcc void @proc_clo$ae48170(%struct.ScmObj* %env$ae48170,%struct.ScmObj* %current_45args54460) {
%stackaddr$prim56312 = alloca %struct.ScmObj*, align 8
%k47515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54460)
store volatile %struct.ScmObj* %k47515, %struct.ScmObj** %stackaddr$prim56312, align 8
%stackaddr$prim56313 = alloca %struct.ScmObj*, align 8
%current_45args54461 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54460)
store volatile %struct.ScmObj* %current_45args54461, %struct.ScmObj** %stackaddr$prim56313, align 8
%stackaddr$prim56314 = alloca %struct.ScmObj*, align 8
%x47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54461)
store volatile %struct.ScmObj* %x47115, %struct.ScmObj** %stackaddr$prim56314, align 8
%stackaddr$prim56315 = alloca %struct.ScmObj*, align 8
%current_45args54462 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54461)
store volatile %struct.ScmObj* %current_45args54462, %struct.ScmObj** %stackaddr$prim56315, align 8
%stackaddr$prim56316 = alloca %struct.ScmObj*, align 8
%y47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54462)
store volatile %struct.ScmObj* %y47114, %struct.ScmObj** %stackaddr$prim56316, align 8
%ae48172 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54464$k475150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56317 = alloca %struct.ScmObj*, align 8
%argslist54464$k475151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47115, %struct.ScmObj* %argslist54464$k475150)
store volatile %struct.ScmObj* %argslist54464$k475151, %struct.ScmObj** %stackaddr$prim56317, align 8
%stackaddr$prim56318 = alloca %struct.ScmObj*, align 8
%argslist54464$k475152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48172, %struct.ScmObj* %argslist54464$k475151)
store volatile %struct.ScmObj* %argslist54464$k475152, %struct.ScmObj** %stackaddr$prim56318, align 8
%clofunc56319 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47515)
musttail call tailcc void %clofunc56319(%struct.ScmObj* %k47515, %struct.ScmObj* %argslist54464$k475152)
ret void
}

define tailcc void @proc_clo$ae48088(%struct.ScmObj* %env$ae48088,%struct.ScmObj* %current_45args54468) {
%stackaddr$prim56320 = alloca %struct.ScmObj*, align 8
%k47516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54468)
store volatile %struct.ScmObj* %k47516, %struct.ScmObj** %stackaddr$prim56320, align 8
%stackaddr$prim56321 = alloca %struct.ScmObj*, align 8
%current_45args54469 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54468)
store volatile %struct.ScmObj* %current_45args54469, %struct.ScmObj** %stackaddr$prim56321, align 8
%stackaddr$prim56322 = alloca %struct.ScmObj*, align 8
%_37foldl147075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54469)
store volatile %struct.ScmObj* %_37foldl147075, %struct.ScmObj** %stackaddr$prim56322, align 8
%ae48090 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56323 = alloca %struct.ScmObj*, align 8
%fptrToInt56324 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48091 to i64
%ae48091 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56324)
store volatile %struct.ScmObj* %ae48091, %struct.ScmObj** %stackaddr$makeclosure56323, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48091, %struct.ScmObj* %_37foldl147075, i64 0)
%argslist54482$k475160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56325 = alloca %struct.ScmObj*, align 8
%argslist54482$k475161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48091, %struct.ScmObj* %argslist54482$k475160)
store volatile %struct.ScmObj* %argslist54482$k475161, %struct.ScmObj** %stackaddr$prim56325, align 8
%stackaddr$prim56326 = alloca %struct.ScmObj*, align 8
%argslist54482$k475162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48090, %struct.ScmObj* %argslist54482$k475161)
store volatile %struct.ScmObj* %argslist54482$k475162, %struct.ScmObj** %stackaddr$prim56326, align 8
%clofunc56327 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47516)
musttail call tailcc void %clofunc56327(%struct.ScmObj* %k47516, %struct.ScmObj* %argslist54482$k475162)
ret void
}

define tailcc void @proc_clo$ae48091(%struct.ScmObj* %env$ae48091,%struct.ScmObj* %current_45args54471) {
%stackaddr$env-ref56328 = alloca %struct.ScmObj*, align 8
%_37foldl147075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48091, i64 0)
store %struct.ScmObj* %_37foldl147075, %struct.ScmObj** %stackaddr$env-ref56328
%stackaddr$prim56329 = alloca %struct.ScmObj*, align 8
%k47517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54471)
store volatile %struct.ScmObj* %k47517, %struct.ScmObj** %stackaddr$prim56329, align 8
%stackaddr$prim56330 = alloca %struct.ScmObj*, align 8
%current_45args54472 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54471)
store volatile %struct.ScmObj* %current_45args54472, %struct.ScmObj** %stackaddr$prim56330, align 8
%stackaddr$prim56331 = alloca %struct.ScmObj*, align 8
%f47078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54472)
store volatile %struct.ScmObj* %f47078, %struct.ScmObj** %stackaddr$prim56331, align 8
%stackaddr$prim56332 = alloca %struct.ScmObj*, align 8
%current_45args54473 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54472)
store volatile %struct.ScmObj* %current_45args54473, %struct.ScmObj** %stackaddr$prim56332, align 8
%stackaddr$prim56333 = alloca %struct.ScmObj*, align 8
%acc47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54473)
store volatile %struct.ScmObj* %acc47077, %struct.ScmObj** %stackaddr$prim56333, align 8
%stackaddr$prim56334 = alloca %struct.ScmObj*, align 8
%current_45args54474 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54473)
store volatile %struct.ScmObj* %current_45args54474, %struct.ScmObj** %stackaddr$prim56334, align 8
%stackaddr$prim56335 = alloca %struct.ScmObj*, align 8
%lst47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54474)
store volatile %struct.ScmObj* %lst47076, %struct.ScmObj** %stackaddr$prim56335, align 8
%stackaddr$prim56336 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47076)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim56336, align 8
%truthy$cmp56337 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47234)
%cmp$cmp56337 = icmp eq i64 %truthy$cmp56337, 1
br i1 %cmp$cmp56337, label %truebranch$cmp56337, label %falsebranch$cmp56337
truebranch$cmp56337:
%ae48095 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54476$k475170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56338 = alloca %struct.ScmObj*, align 8
%argslist54476$k475171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47077, %struct.ScmObj* %argslist54476$k475170)
store volatile %struct.ScmObj* %argslist54476$k475171, %struct.ScmObj** %stackaddr$prim56338, align 8
%stackaddr$prim56339 = alloca %struct.ScmObj*, align 8
%argslist54476$k475172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48095, %struct.ScmObj* %argslist54476$k475171)
store volatile %struct.ScmObj* %argslist54476$k475172, %struct.ScmObj** %stackaddr$prim56339, align 8
%clofunc56340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47517)
musttail call tailcc void %clofunc56340(%struct.ScmObj* %k47517, %struct.ScmObj* %argslist54476$k475172)
ret void
falsebranch$cmp56337:
%stackaddr$prim56341 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47076)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim56341, align 8
%stackaddr$makeclosure56342 = alloca %struct.ScmObj*, align 8
%fptrToInt56343 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48102 to i64
%ae48102 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56343)
store volatile %struct.ScmObj* %ae48102, %struct.ScmObj** %stackaddr$makeclosure56342, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48102, %struct.ScmObj* %f47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48102, %struct.ScmObj* %lst47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48102, %struct.ScmObj* %_37foldl147075, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48102, %struct.ScmObj* %k47517, i64 3)
%argslist54481$f470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56344 = alloca %struct.ScmObj*, align 8
%argslist54481$f470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47077, %struct.ScmObj* %argslist54481$f470780)
store volatile %struct.ScmObj* %argslist54481$f470781, %struct.ScmObj** %stackaddr$prim56344, align 8
%stackaddr$prim56345 = alloca %struct.ScmObj*, align 8
%argslist54481$f470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47235, %struct.ScmObj* %argslist54481$f470781)
store volatile %struct.ScmObj* %argslist54481$f470782, %struct.ScmObj** %stackaddr$prim56345, align 8
%stackaddr$prim56346 = alloca %struct.ScmObj*, align 8
%argslist54481$f470783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48102, %struct.ScmObj* %argslist54481$f470782)
store volatile %struct.ScmObj* %argslist54481$f470783, %struct.ScmObj** %stackaddr$prim56346, align 8
%clofunc56347 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47078)
musttail call tailcc void %clofunc56347(%struct.ScmObj* %f47078, %struct.ScmObj* %argslist54481$f470783)
ret void
}

define tailcc void @proc_clo$ae48102(%struct.ScmObj* %env$ae48102,%struct.ScmObj* %current_45args54477) {
%stackaddr$env-ref56348 = alloca %struct.ScmObj*, align 8
%f47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48102, i64 0)
store %struct.ScmObj* %f47078, %struct.ScmObj** %stackaddr$env-ref56348
%stackaddr$env-ref56349 = alloca %struct.ScmObj*, align 8
%lst47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48102, i64 1)
store %struct.ScmObj* %lst47076, %struct.ScmObj** %stackaddr$env-ref56349
%stackaddr$env-ref56350 = alloca %struct.ScmObj*, align 8
%_37foldl147075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48102, i64 2)
store %struct.ScmObj* %_37foldl147075, %struct.ScmObj** %stackaddr$env-ref56350
%stackaddr$env-ref56351 = alloca %struct.ScmObj*, align 8
%k47517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48102, i64 3)
store %struct.ScmObj* %k47517, %struct.ScmObj** %stackaddr$env-ref56351
%stackaddr$prim56352 = alloca %struct.ScmObj*, align 8
%_95k47518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54477)
store volatile %struct.ScmObj* %_95k47518, %struct.ScmObj** %stackaddr$prim56352, align 8
%stackaddr$prim56353 = alloca %struct.ScmObj*, align 8
%current_45args54478 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54477)
store volatile %struct.ScmObj* %current_45args54478, %struct.ScmObj** %stackaddr$prim56353, align 8
%stackaddr$prim56354 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54478)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim56354, align 8
%stackaddr$prim56355 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47076)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim56355, align 8
%argslist54480$_37foldl1470750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56356 = alloca %struct.ScmObj*, align 8
%argslist54480$_37foldl1470751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47237, %struct.ScmObj* %argslist54480$_37foldl1470750)
store volatile %struct.ScmObj* %argslist54480$_37foldl1470751, %struct.ScmObj** %stackaddr$prim56356, align 8
%stackaddr$prim56357 = alloca %struct.ScmObj*, align 8
%argslist54480$_37foldl1470752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47236, %struct.ScmObj* %argslist54480$_37foldl1470751)
store volatile %struct.ScmObj* %argslist54480$_37foldl1470752, %struct.ScmObj** %stackaddr$prim56357, align 8
%stackaddr$prim56358 = alloca %struct.ScmObj*, align 8
%argslist54480$_37foldl1470753 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47078, %struct.ScmObj* %argslist54480$_37foldl1470752)
store volatile %struct.ScmObj* %argslist54480$_37foldl1470753, %struct.ScmObj** %stackaddr$prim56358, align 8
%stackaddr$prim56359 = alloca %struct.ScmObj*, align 8
%argslist54480$_37foldl1470754 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47517, %struct.ScmObj* %argslist54480$_37foldl1470753)
store volatile %struct.ScmObj* %argslist54480$_37foldl1470754, %struct.ScmObj** %stackaddr$prim56359, align 8
%clofunc56360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147075)
musttail call tailcc void %clofunc56360(%struct.ScmObj* %_37foldl147075, %struct.ScmObj* %argslist54480$_37foldl1470754)
ret void
}

define tailcc void @proc_clo$ae48005(%struct.ScmObj* %env$ae48005,%struct.ScmObj* %current_45args54485) {
%stackaddr$prim56361 = alloca %struct.ScmObj*, align 8
%k47519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54485)
store volatile %struct.ScmObj* %k47519, %struct.ScmObj** %stackaddr$prim56361, align 8
%stackaddr$prim56362 = alloca %struct.ScmObj*, align 8
%current_45args54486 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54485)
store volatile %struct.ScmObj* %current_45args54486, %struct.ScmObj** %stackaddr$prim56362, align 8
%stackaddr$prim56363 = alloca %struct.ScmObj*, align 8
%_37length47080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54486)
store volatile %struct.ScmObj* %_37length47080, %struct.ScmObj** %stackaddr$prim56363, align 8
%ae48007 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56364 = alloca %struct.ScmObj*, align 8
%fptrToInt56365 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48008 to i64
%ae48008 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56365)
store volatile %struct.ScmObj* %ae48008, %struct.ScmObj** %stackaddr$makeclosure56364, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48008, %struct.ScmObj* %_37length47080, i64 0)
%argslist54497$k475190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56366 = alloca %struct.ScmObj*, align 8
%argslist54497$k475191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48008, %struct.ScmObj* %argslist54497$k475190)
store volatile %struct.ScmObj* %argslist54497$k475191, %struct.ScmObj** %stackaddr$prim56366, align 8
%stackaddr$prim56367 = alloca %struct.ScmObj*, align 8
%argslist54497$k475192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48007, %struct.ScmObj* %argslist54497$k475191)
store volatile %struct.ScmObj* %argslist54497$k475192, %struct.ScmObj** %stackaddr$prim56367, align 8
%clofunc56368 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47519)
musttail call tailcc void %clofunc56368(%struct.ScmObj* %k47519, %struct.ScmObj* %argslist54497$k475192)
ret void
}

define tailcc void @proc_clo$ae48008(%struct.ScmObj* %env$ae48008,%struct.ScmObj* %current_45args54488) {
%stackaddr$env-ref56369 = alloca %struct.ScmObj*, align 8
%_37length47080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48008, i64 0)
store %struct.ScmObj* %_37length47080, %struct.ScmObj** %stackaddr$env-ref56369
%stackaddr$prim56370 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54488)
store volatile %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$prim56370, align 8
%stackaddr$prim56371 = alloca %struct.ScmObj*, align 8
%current_45args54489 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54488)
store volatile %struct.ScmObj* %current_45args54489, %struct.ScmObj** %stackaddr$prim56371, align 8
%stackaddr$prim56372 = alloca %struct.ScmObj*, align 8
%lst47081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54489)
store volatile %struct.ScmObj* %lst47081, %struct.ScmObj** %stackaddr$prim56372, align 8
%stackaddr$prim56373 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47081)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim56373, align 8
%truthy$cmp56374 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47230)
%cmp$cmp56374 = icmp eq i64 %truthy$cmp56374, 1
br i1 %cmp$cmp56374, label %truebranch$cmp56374, label %falsebranch$cmp56374
truebranch$cmp56374:
%ae48012 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48013 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54491$k475200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56375 = alloca %struct.ScmObj*, align 8
%argslist54491$k475201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48013, %struct.ScmObj* %argslist54491$k475200)
store volatile %struct.ScmObj* %argslist54491$k475201, %struct.ScmObj** %stackaddr$prim56375, align 8
%stackaddr$prim56376 = alloca %struct.ScmObj*, align 8
%argslist54491$k475202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48012, %struct.ScmObj* %argslist54491$k475201)
store volatile %struct.ScmObj* %argslist54491$k475202, %struct.ScmObj** %stackaddr$prim56376, align 8
%clofunc56377 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47520)
musttail call tailcc void %clofunc56377(%struct.ScmObj* %k47520, %struct.ScmObj* %argslist54491$k475202)
ret void
falsebranch$cmp56374:
%stackaddr$prim56378 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47081)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim56378, align 8
%stackaddr$makeclosure56379 = alloca %struct.ScmObj*, align 8
%fptrToInt56380 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48022 to i64
%ae48022 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56380)
store volatile %struct.ScmObj* %ae48022, %struct.ScmObj** %stackaddr$makeclosure56379, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48022, %struct.ScmObj* %k47520, i64 0)
%argslist54496$_37length470800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56381 = alloca %struct.ScmObj*, align 8
%argslist54496$_37length470801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47231, %struct.ScmObj* %argslist54496$_37length470800)
store volatile %struct.ScmObj* %argslist54496$_37length470801, %struct.ScmObj** %stackaddr$prim56381, align 8
%stackaddr$prim56382 = alloca %struct.ScmObj*, align 8
%argslist54496$_37length470802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48022, %struct.ScmObj* %argslist54496$_37length470801)
store volatile %struct.ScmObj* %argslist54496$_37length470802, %struct.ScmObj** %stackaddr$prim56382, align 8
%clofunc56383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47080)
musttail call tailcc void %clofunc56383(%struct.ScmObj* %_37length47080, %struct.ScmObj* %argslist54496$_37length470802)
ret void
}

define tailcc void @proc_clo$ae48022(%struct.ScmObj* %env$ae48022,%struct.ScmObj* %current_45args54492) {
%stackaddr$env-ref56384 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48022, i64 0)
store %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$env-ref56384
%stackaddr$prim56385 = alloca %struct.ScmObj*, align 8
%_95k47521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54492)
store volatile %struct.ScmObj* %_95k47521, %struct.ScmObj** %stackaddr$prim56385, align 8
%stackaddr$prim56386 = alloca %struct.ScmObj*, align 8
%current_45args54493 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54492)
store volatile %struct.ScmObj* %current_45args54493, %struct.ScmObj** %stackaddr$prim56386, align 8
%stackaddr$prim56387 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54493)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim56387, align 8
%ae48024 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56388 = alloca %struct.ScmObj*, align 8
%cpsprim47522 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48024, %struct.ScmObj* %anf_45bind47232)
store volatile %struct.ScmObj* %cpsprim47522, %struct.ScmObj** %stackaddr$prim56388, align 8
%ae48027 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54495$k475200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56389 = alloca %struct.ScmObj*, align 8
%argslist54495$k475201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47522, %struct.ScmObj* %argslist54495$k475200)
store volatile %struct.ScmObj* %argslist54495$k475201, %struct.ScmObj** %stackaddr$prim56389, align 8
%stackaddr$prim56390 = alloca %struct.ScmObj*, align 8
%argslist54495$k475202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48027, %struct.ScmObj* %argslist54495$k475201)
store volatile %struct.ScmObj* %argslist54495$k475202, %struct.ScmObj** %stackaddr$prim56390, align 8
%clofunc56391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47520)
musttail call tailcc void %clofunc56391(%struct.ScmObj* %k47520, %struct.ScmObj* %argslist54495$k475202)
ret void
}

define tailcc void @proc_clo$ae47855(%struct.ScmObj* %env$ae47855,%struct.ScmObj* %current_45args54500) {
%stackaddr$prim56392 = alloca %struct.ScmObj*, align 8
%k47523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54500)
store volatile %struct.ScmObj* %k47523, %struct.ScmObj** %stackaddr$prim56392, align 8
%stackaddr$prim56393 = alloca %struct.ScmObj*, align 8
%current_45args54501 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54500)
store volatile %struct.ScmObj* %current_45args54501, %struct.ScmObj** %stackaddr$prim56393, align 8
%stackaddr$prim56394 = alloca %struct.ScmObj*, align 8
%_37take47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54501)
store volatile %struct.ScmObj* %_37take47083, %struct.ScmObj** %stackaddr$prim56394, align 8
%ae47857 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56395 = alloca %struct.ScmObj*, align 8
%fptrToInt56396 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47858 to i64
%ae47858 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56396)
store volatile %struct.ScmObj* %ae47858, %struct.ScmObj** %stackaddr$makeclosure56395, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47858, %struct.ScmObj* %_37take47083, i64 0)
%argslist54514$k475230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56397 = alloca %struct.ScmObj*, align 8
%argslist54514$k475231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47858, %struct.ScmObj* %argslist54514$k475230)
store volatile %struct.ScmObj* %argslist54514$k475231, %struct.ScmObj** %stackaddr$prim56397, align 8
%stackaddr$prim56398 = alloca %struct.ScmObj*, align 8
%argslist54514$k475232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47857, %struct.ScmObj* %argslist54514$k475231)
store volatile %struct.ScmObj* %argslist54514$k475232, %struct.ScmObj** %stackaddr$prim56398, align 8
%clofunc56399 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47523)
musttail call tailcc void %clofunc56399(%struct.ScmObj* %k47523, %struct.ScmObj* %argslist54514$k475232)
ret void
}

define tailcc void @proc_clo$ae47858(%struct.ScmObj* %env$ae47858,%struct.ScmObj* %current_45args54503) {
%stackaddr$env-ref56400 = alloca %struct.ScmObj*, align 8
%_37take47083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47858, i64 0)
store %struct.ScmObj* %_37take47083, %struct.ScmObj** %stackaddr$env-ref56400
%stackaddr$prim56401 = alloca %struct.ScmObj*, align 8
%k47524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54503)
store volatile %struct.ScmObj* %k47524, %struct.ScmObj** %stackaddr$prim56401, align 8
%stackaddr$prim56402 = alloca %struct.ScmObj*, align 8
%current_45args54504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54503)
store volatile %struct.ScmObj* %current_45args54504, %struct.ScmObj** %stackaddr$prim56402, align 8
%stackaddr$prim56403 = alloca %struct.ScmObj*, align 8
%lst47085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54504)
store volatile %struct.ScmObj* %lst47085, %struct.ScmObj** %stackaddr$prim56403, align 8
%stackaddr$prim56404 = alloca %struct.ScmObj*, align 8
%current_45args54505 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54504)
store volatile %struct.ScmObj* %current_45args54505, %struct.ScmObj** %stackaddr$prim56404, align 8
%stackaddr$prim56405 = alloca %struct.ScmObj*, align 8
%n47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54505)
store volatile %struct.ScmObj* %n47084, %struct.ScmObj** %stackaddr$prim56405, align 8
%ae47860 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56406 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47084, %struct.ScmObj* %ae47860)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim56406, align 8
%truthy$cmp56407 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47223)
%cmp$cmp56407 = icmp eq i64 %truthy$cmp56407, 1
br i1 %cmp$cmp56407, label %truebranch$cmp56407, label %falsebranch$cmp56407
truebranch$cmp56407:
%ae47863 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47864 = call %struct.ScmObj* @const_init_null()
%argslist54507$k475240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56408 = alloca %struct.ScmObj*, align 8
%argslist54507$k475241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47864, %struct.ScmObj* %argslist54507$k475240)
store volatile %struct.ScmObj* %argslist54507$k475241, %struct.ScmObj** %stackaddr$prim56408, align 8
%stackaddr$prim56409 = alloca %struct.ScmObj*, align 8
%argslist54507$k475242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47863, %struct.ScmObj* %argslist54507$k475241)
store volatile %struct.ScmObj* %argslist54507$k475242, %struct.ScmObj** %stackaddr$prim56409, align 8
%clofunc56410 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47524)
musttail call tailcc void %clofunc56410(%struct.ScmObj* %k47524, %struct.ScmObj* %argslist54507$k475242)
ret void
falsebranch$cmp56407:
%stackaddr$prim56411 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47085)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim56411, align 8
%truthy$cmp56412 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47224)
%cmp$cmp56412 = icmp eq i64 %truthy$cmp56412, 1
br i1 %cmp$cmp56412, label %truebranch$cmp56412, label %falsebranch$cmp56412
truebranch$cmp56412:
%ae47874 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47875 = call %struct.ScmObj* @const_init_null()
%argslist54508$k475240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56413 = alloca %struct.ScmObj*, align 8
%argslist54508$k475241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47875, %struct.ScmObj* %argslist54508$k475240)
store volatile %struct.ScmObj* %argslist54508$k475241, %struct.ScmObj** %stackaddr$prim56413, align 8
%stackaddr$prim56414 = alloca %struct.ScmObj*, align 8
%argslist54508$k475242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47874, %struct.ScmObj* %argslist54508$k475241)
store volatile %struct.ScmObj* %argslist54508$k475242, %struct.ScmObj** %stackaddr$prim56414, align 8
%clofunc56415 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47524)
musttail call tailcc void %clofunc56415(%struct.ScmObj* %k47524, %struct.ScmObj* %argslist54508$k475242)
ret void
falsebranch$cmp56412:
%stackaddr$prim56416 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47085)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim56416, align 8
%stackaddr$prim56417 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47085)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim56417, align 8
%ae47885 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56418 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47084, %struct.ScmObj* %ae47885)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim56418, align 8
%stackaddr$makeclosure56419 = alloca %struct.ScmObj*, align 8
%fptrToInt56420 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47887 to i64
%ae47887 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56420)
store volatile %struct.ScmObj* %ae47887, %struct.ScmObj** %stackaddr$makeclosure56419, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47887, %struct.ScmObj* %k47524, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47887, %struct.ScmObj* %anf_45bind47225, i64 1)
%argslist54513$_37take470830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56421 = alloca %struct.ScmObj*, align 8
%argslist54513$_37take470831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47227, %struct.ScmObj* %argslist54513$_37take470830)
store volatile %struct.ScmObj* %argslist54513$_37take470831, %struct.ScmObj** %stackaddr$prim56421, align 8
%stackaddr$prim56422 = alloca %struct.ScmObj*, align 8
%argslist54513$_37take470832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47226, %struct.ScmObj* %argslist54513$_37take470831)
store volatile %struct.ScmObj* %argslist54513$_37take470832, %struct.ScmObj** %stackaddr$prim56422, align 8
%stackaddr$prim56423 = alloca %struct.ScmObj*, align 8
%argslist54513$_37take470833 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47887, %struct.ScmObj* %argslist54513$_37take470832)
store volatile %struct.ScmObj* %argslist54513$_37take470833, %struct.ScmObj** %stackaddr$prim56423, align 8
%clofunc56424 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47083)
musttail call tailcc void %clofunc56424(%struct.ScmObj* %_37take47083, %struct.ScmObj* %argslist54513$_37take470833)
ret void
}

define tailcc void @proc_clo$ae47887(%struct.ScmObj* %env$ae47887,%struct.ScmObj* %current_45args54509) {
%stackaddr$env-ref56425 = alloca %struct.ScmObj*, align 8
%k47524 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47887, i64 0)
store %struct.ScmObj* %k47524, %struct.ScmObj** %stackaddr$env-ref56425
%stackaddr$env-ref56426 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47887, i64 1)
store %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$env-ref56426
%stackaddr$prim56427 = alloca %struct.ScmObj*, align 8
%_95k47525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54509)
store volatile %struct.ScmObj* %_95k47525, %struct.ScmObj** %stackaddr$prim56427, align 8
%stackaddr$prim56428 = alloca %struct.ScmObj*, align 8
%current_45args54510 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54509)
store volatile %struct.ScmObj* %current_45args54510, %struct.ScmObj** %stackaddr$prim56428, align 8
%stackaddr$prim56429 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54510)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim56429, align 8
%stackaddr$prim56430 = alloca %struct.ScmObj*, align 8
%cpsprim47526 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47225, %struct.ScmObj* %anf_45bind47228)
store volatile %struct.ScmObj* %cpsprim47526, %struct.ScmObj** %stackaddr$prim56430, align 8
%ae47893 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54512$k475240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56431 = alloca %struct.ScmObj*, align 8
%argslist54512$k475241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47526, %struct.ScmObj* %argslist54512$k475240)
store volatile %struct.ScmObj* %argslist54512$k475241, %struct.ScmObj** %stackaddr$prim56431, align 8
%stackaddr$prim56432 = alloca %struct.ScmObj*, align 8
%argslist54512$k475242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47893, %struct.ScmObj* %argslist54512$k475241)
store volatile %struct.ScmObj* %argslist54512$k475242, %struct.ScmObj** %stackaddr$prim56432, align 8
%clofunc56433 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47524)
musttail call tailcc void %clofunc56433(%struct.ScmObj* %k47524, %struct.ScmObj* %argslist54512$k475242)
ret void
}

define tailcc void @proc_clo$ae47758(%struct.ScmObj* %env$ae47758,%struct.ScmObj* %current_45args54517) {
%stackaddr$prim56434 = alloca %struct.ScmObj*, align 8
%k47527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54517)
store volatile %struct.ScmObj* %k47527, %struct.ScmObj** %stackaddr$prim56434, align 8
%stackaddr$prim56435 = alloca %struct.ScmObj*, align 8
%current_45args54518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54517)
store volatile %struct.ScmObj* %current_45args54518, %struct.ScmObj** %stackaddr$prim56435, align 8
%stackaddr$prim56436 = alloca %struct.ScmObj*, align 8
%_37map47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54518)
store volatile %struct.ScmObj* %_37map47087, %struct.ScmObj** %stackaddr$prim56436, align 8
%ae47760 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56437 = alloca %struct.ScmObj*, align 8
%fptrToInt56438 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47761 to i64
%ae47761 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56438)
store volatile %struct.ScmObj* %ae47761, %struct.ScmObj** %stackaddr$makeclosure56437, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47761, %struct.ScmObj* %_37map47087, i64 0)
%argslist54534$k475270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56439 = alloca %struct.ScmObj*, align 8
%argslist54534$k475271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47761, %struct.ScmObj* %argslist54534$k475270)
store volatile %struct.ScmObj* %argslist54534$k475271, %struct.ScmObj** %stackaddr$prim56439, align 8
%stackaddr$prim56440 = alloca %struct.ScmObj*, align 8
%argslist54534$k475272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47760, %struct.ScmObj* %argslist54534$k475271)
store volatile %struct.ScmObj* %argslist54534$k475272, %struct.ScmObj** %stackaddr$prim56440, align 8
%clofunc56441 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47527)
musttail call tailcc void %clofunc56441(%struct.ScmObj* %k47527, %struct.ScmObj* %argslist54534$k475272)
ret void
}

define tailcc void @proc_clo$ae47761(%struct.ScmObj* %env$ae47761,%struct.ScmObj* %current_45args54520) {
%stackaddr$env-ref56442 = alloca %struct.ScmObj*, align 8
%_37map47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47761, i64 0)
store %struct.ScmObj* %_37map47087, %struct.ScmObj** %stackaddr$env-ref56442
%stackaddr$prim56443 = alloca %struct.ScmObj*, align 8
%k47528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54520)
store volatile %struct.ScmObj* %k47528, %struct.ScmObj** %stackaddr$prim56443, align 8
%stackaddr$prim56444 = alloca %struct.ScmObj*, align 8
%current_45args54521 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54520)
store volatile %struct.ScmObj* %current_45args54521, %struct.ScmObj** %stackaddr$prim56444, align 8
%stackaddr$prim56445 = alloca %struct.ScmObj*, align 8
%f47089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54521)
store volatile %struct.ScmObj* %f47089, %struct.ScmObj** %stackaddr$prim56445, align 8
%stackaddr$prim56446 = alloca %struct.ScmObj*, align 8
%current_45args54522 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54521)
store volatile %struct.ScmObj* %current_45args54522, %struct.ScmObj** %stackaddr$prim56446, align 8
%stackaddr$prim56447 = alloca %struct.ScmObj*, align 8
%lst47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54522)
store volatile %struct.ScmObj* %lst47088, %struct.ScmObj** %stackaddr$prim56447, align 8
%stackaddr$prim56448 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim56448, align 8
%truthy$cmp56449 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47217)
%cmp$cmp56449 = icmp eq i64 %truthy$cmp56449, 1
br i1 %cmp$cmp56449, label %truebranch$cmp56449, label %falsebranch$cmp56449
truebranch$cmp56449:
%ae47765 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47766 = call %struct.ScmObj* @const_init_null()
%argslist54524$k475280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56450 = alloca %struct.ScmObj*, align 8
%argslist54524$k475281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47766, %struct.ScmObj* %argslist54524$k475280)
store volatile %struct.ScmObj* %argslist54524$k475281, %struct.ScmObj** %stackaddr$prim56450, align 8
%stackaddr$prim56451 = alloca %struct.ScmObj*, align 8
%argslist54524$k475282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47765, %struct.ScmObj* %argslist54524$k475281)
store volatile %struct.ScmObj* %argslist54524$k475282, %struct.ScmObj** %stackaddr$prim56451, align 8
%clofunc56452 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47528)
musttail call tailcc void %clofunc56452(%struct.ScmObj* %k47528, %struct.ScmObj* %argslist54524$k475282)
ret void
falsebranch$cmp56449:
%stackaddr$prim56453 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim56453, align 8
%stackaddr$makeclosure56454 = alloca %struct.ScmObj*, align 8
%fptrToInt56455 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47775 to i64
%ae47775 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56455)
store volatile %struct.ScmObj* %ae47775, %struct.ScmObj** %stackaddr$makeclosure56454, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47775, %struct.ScmObj* %f47089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47775, %struct.ScmObj* %lst47088, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47775, %struct.ScmObj* %_37map47087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47775, %struct.ScmObj* %k47528, i64 3)
%argslist54533$f470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56456 = alloca %struct.ScmObj*, align 8
%argslist54533$f470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47218, %struct.ScmObj* %argslist54533$f470890)
store volatile %struct.ScmObj* %argslist54533$f470891, %struct.ScmObj** %stackaddr$prim56456, align 8
%stackaddr$prim56457 = alloca %struct.ScmObj*, align 8
%argslist54533$f470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47775, %struct.ScmObj* %argslist54533$f470891)
store volatile %struct.ScmObj* %argslist54533$f470892, %struct.ScmObj** %stackaddr$prim56457, align 8
%clofunc56458 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47089)
musttail call tailcc void %clofunc56458(%struct.ScmObj* %f47089, %struct.ScmObj* %argslist54533$f470892)
ret void
}

define tailcc void @proc_clo$ae47775(%struct.ScmObj* %env$ae47775,%struct.ScmObj* %current_45args54525) {
%stackaddr$env-ref56459 = alloca %struct.ScmObj*, align 8
%f47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47775, i64 0)
store %struct.ScmObj* %f47089, %struct.ScmObj** %stackaddr$env-ref56459
%stackaddr$env-ref56460 = alloca %struct.ScmObj*, align 8
%lst47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47775, i64 1)
store %struct.ScmObj* %lst47088, %struct.ScmObj** %stackaddr$env-ref56460
%stackaddr$env-ref56461 = alloca %struct.ScmObj*, align 8
%_37map47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47775, i64 2)
store %struct.ScmObj* %_37map47087, %struct.ScmObj** %stackaddr$env-ref56461
%stackaddr$env-ref56462 = alloca %struct.ScmObj*, align 8
%k47528 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47775, i64 3)
store %struct.ScmObj* %k47528, %struct.ScmObj** %stackaddr$env-ref56462
%stackaddr$prim56463 = alloca %struct.ScmObj*, align 8
%_95k47529 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54525)
store volatile %struct.ScmObj* %_95k47529, %struct.ScmObj** %stackaddr$prim56463, align 8
%stackaddr$prim56464 = alloca %struct.ScmObj*, align 8
%current_45args54526 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54525)
store volatile %struct.ScmObj* %current_45args54526, %struct.ScmObj** %stackaddr$prim56464, align 8
%stackaddr$prim56465 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54526)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim56465, align 8
%stackaddr$prim56466 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim56466, align 8
%stackaddr$makeclosure56467 = alloca %struct.ScmObj*, align 8
%fptrToInt56468 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47779 to i64
%ae47779 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56468)
store volatile %struct.ScmObj* %ae47779, %struct.ScmObj** %stackaddr$makeclosure56467, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47779, %struct.ScmObj* %anf_45bind47219, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47779, %struct.ScmObj* %k47528, i64 1)
%argslist54532$_37map470870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56469 = alloca %struct.ScmObj*, align 8
%argslist54532$_37map470871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47220, %struct.ScmObj* %argslist54532$_37map470870)
store volatile %struct.ScmObj* %argslist54532$_37map470871, %struct.ScmObj** %stackaddr$prim56469, align 8
%stackaddr$prim56470 = alloca %struct.ScmObj*, align 8
%argslist54532$_37map470872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47089, %struct.ScmObj* %argslist54532$_37map470871)
store volatile %struct.ScmObj* %argslist54532$_37map470872, %struct.ScmObj** %stackaddr$prim56470, align 8
%stackaddr$prim56471 = alloca %struct.ScmObj*, align 8
%argslist54532$_37map470873 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47779, %struct.ScmObj* %argslist54532$_37map470872)
store volatile %struct.ScmObj* %argslist54532$_37map470873, %struct.ScmObj** %stackaddr$prim56471, align 8
%clofunc56472 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47087)
musttail call tailcc void %clofunc56472(%struct.ScmObj* %_37map47087, %struct.ScmObj* %argslist54532$_37map470873)
ret void
}

define tailcc void @proc_clo$ae47779(%struct.ScmObj* %env$ae47779,%struct.ScmObj* %current_45args54528) {
%stackaddr$env-ref56473 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47779, i64 0)
store %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$env-ref56473
%stackaddr$env-ref56474 = alloca %struct.ScmObj*, align 8
%k47528 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47779, i64 1)
store %struct.ScmObj* %k47528, %struct.ScmObj** %stackaddr$env-ref56474
%stackaddr$prim56475 = alloca %struct.ScmObj*, align 8
%_95k47530 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54528)
store volatile %struct.ScmObj* %_95k47530, %struct.ScmObj** %stackaddr$prim56475, align 8
%stackaddr$prim56476 = alloca %struct.ScmObj*, align 8
%current_45args54529 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54528)
store volatile %struct.ScmObj* %current_45args54529, %struct.ScmObj** %stackaddr$prim56476, align 8
%stackaddr$prim56477 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54529)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim56477, align 8
%stackaddr$prim56478 = alloca %struct.ScmObj*, align 8
%cpsprim47531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47219, %struct.ScmObj* %anf_45bind47221)
store volatile %struct.ScmObj* %cpsprim47531, %struct.ScmObj** %stackaddr$prim56478, align 8
%ae47785 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54531$k475280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56479 = alloca %struct.ScmObj*, align 8
%argslist54531$k475281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47531, %struct.ScmObj* %argslist54531$k475280)
store volatile %struct.ScmObj* %argslist54531$k475281, %struct.ScmObj** %stackaddr$prim56479, align 8
%stackaddr$prim56480 = alloca %struct.ScmObj*, align 8
%argslist54531$k475282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47785, %struct.ScmObj* %argslist54531$k475281)
store volatile %struct.ScmObj* %argslist54531$k475282, %struct.ScmObj** %stackaddr$prim56480, align 8
%clofunc56481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47528)
musttail call tailcc void %clofunc56481(%struct.ScmObj* %k47528, %struct.ScmObj* %argslist54531$k475282)
ret void
}

define tailcc void @proc_clo$ae47678(%struct.ScmObj* %env$ae47678,%struct.ScmObj* %current_45args54537) {
%stackaddr$prim56482 = alloca %struct.ScmObj*, align 8
%k47532 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54537)
store volatile %struct.ScmObj* %k47532, %struct.ScmObj** %stackaddr$prim56482, align 8
%stackaddr$prim56483 = alloca %struct.ScmObj*, align 8
%current_45args54538 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54537)
store volatile %struct.ScmObj* %current_45args54538, %struct.ScmObj** %stackaddr$prim56483, align 8
%stackaddr$prim56484 = alloca %struct.ScmObj*, align 8
%_37foldr147091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54538)
store volatile %struct.ScmObj* %_37foldr147091, %struct.ScmObj** %stackaddr$prim56484, align 8
%ae47680 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56485 = alloca %struct.ScmObj*, align 8
%fptrToInt56486 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47681 to i64
%ae47681 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56486)
store volatile %struct.ScmObj* %ae47681, %struct.ScmObj** %stackaddr$makeclosure56485, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47681, %struct.ScmObj* %_37foldr147091, i64 0)
%argslist54551$k475320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56487 = alloca %struct.ScmObj*, align 8
%argslist54551$k475321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47681, %struct.ScmObj* %argslist54551$k475320)
store volatile %struct.ScmObj* %argslist54551$k475321, %struct.ScmObj** %stackaddr$prim56487, align 8
%stackaddr$prim56488 = alloca %struct.ScmObj*, align 8
%argslist54551$k475322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47680, %struct.ScmObj* %argslist54551$k475321)
store volatile %struct.ScmObj* %argslist54551$k475322, %struct.ScmObj** %stackaddr$prim56488, align 8
%clofunc56489 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47532)
musttail call tailcc void %clofunc56489(%struct.ScmObj* %k47532, %struct.ScmObj* %argslist54551$k475322)
ret void
}

define tailcc void @proc_clo$ae47681(%struct.ScmObj* %env$ae47681,%struct.ScmObj* %current_45args54540) {
%stackaddr$env-ref56490 = alloca %struct.ScmObj*, align 8
%_37foldr147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47681, i64 0)
store %struct.ScmObj* %_37foldr147091, %struct.ScmObj** %stackaddr$env-ref56490
%stackaddr$prim56491 = alloca %struct.ScmObj*, align 8
%k47533 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54540)
store volatile %struct.ScmObj* %k47533, %struct.ScmObj** %stackaddr$prim56491, align 8
%stackaddr$prim56492 = alloca %struct.ScmObj*, align 8
%current_45args54541 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54540)
store volatile %struct.ScmObj* %current_45args54541, %struct.ScmObj** %stackaddr$prim56492, align 8
%stackaddr$prim56493 = alloca %struct.ScmObj*, align 8
%f47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54541)
store volatile %struct.ScmObj* %f47094, %struct.ScmObj** %stackaddr$prim56493, align 8
%stackaddr$prim56494 = alloca %struct.ScmObj*, align 8
%current_45args54542 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54541)
store volatile %struct.ScmObj* %current_45args54542, %struct.ScmObj** %stackaddr$prim56494, align 8
%stackaddr$prim56495 = alloca %struct.ScmObj*, align 8
%acc47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54542)
store volatile %struct.ScmObj* %acc47093, %struct.ScmObj** %stackaddr$prim56495, align 8
%stackaddr$prim56496 = alloca %struct.ScmObj*, align 8
%current_45args54543 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54542)
store volatile %struct.ScmObj* %current_45args54543, %struct.ScmObj** %stackaddr$prim56496, align 8
%stackaddr$prim56497 = alloca %struct.ScmObj*, align 8
%lst47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54543)
store volatile %struct.ScmObj* %lst47092, %struct.ScmObj** %stackaddr$prim56497, align 8
%stackaddr$prim56498 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$prim56498, align 8
%truthy$cmp56499 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47212)
%cmp$cmp56499 = icmp eq i64 %truthy$cmp56499, 1
br i1 %cmp$cmp56499, label %truebranch$cmp56499, label %falsebranch$cmp56499
truebranch$cmp56499:
%ae47685 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54545$k475330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56500 = alloca %struct.ScmObj*, align 8
%argslist54545$k475331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47093, %struct.ScmObj* %argslist54545$k475330)
store volatile %struct.ScmObj* %argslist54545$k475331, %struct.ScmObj** %stackaddr$prim56500, align 8
%stackaddr$prim56501 = alloca %struct.ScmObj*, align 8
%argslist54545$k475332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47685, %struct.ScmObj* %argslist54545$k475331)
store volatile %struct.ScmObj* %argslist54545$k475332, %struct.ScmObj** %stackaddr$prim56501, align 8
%clofunc56502 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47533)
musttail call tailcc void %clofunc56502(%struct.ScmObj* %k47533, %struct.ScmObj* %argslist54545$k475332)
ret void
falsebranch$cmp56499:
%stackaddr$prim56503 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$prim56503, align 8
%stackaddr$prim56504 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$prim56504, align 8
%stackaddr$makeclosure56505 = alloca %struct.ScmObj*, align 8
%fptrToInt56506 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47693 to i64
%ae47693 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56506)
store volatile %struct.ScmObj* %ae47693, %struct.ScmObj** %stackaddr$makeclosure56505, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47693, %struct.ScmObj* %anf_45bind47213, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47693, %struct.ScmObj* %k47533, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47693, %struct.ScmObj* %f47094, i64 2)
%argslist54550$_37foldr1470910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56507 = alloca %struct.ScmObj*, align 8
%argslist54550$_37foldr1470911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47214, %struct.ScmObj* %argslist54550$_37foldr1470910)
store volatile %struct.ScmObj* %argslist54550$_37foldr1470911, %struct.ScmObj** %stackaddr$prim56507, align 8
%stackaddr$prim56508 = alloca %struct.ScmObj*, align 8
%argslist54550$_37foldr1470912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47093, %struct.ScmObj* %argslist54550$_37foldr1470911)
store volatile %struct.ScmObj* %argslist54550$_37foldr1470912, %struct.ScmObj** %stackaddr$prim56508, align 8
%stackaddr$prim56509 = alloca %struct.ScmObj*, align 8
%argslist54550$_37foldr1470913 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47094, %struct.ScmObj* %argslist54550$_37foldr1470912)
store volatile %struct.ScmObj* %argslist54550$_37foldr1470913, %struct.ScmObj** %stackaddr$prim56509, align 8
%stackaddr$prim56510 = alloca %struct.ScmObj*, align 8
%argslist54550$_37foldr1470914 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47693, %struct.ScmObj* %argslist54550$_37foldr1470913)
store volatile %struct.ScmObj* %argslist54550$_37foldr1470914, %struct.ScmObj** %stackaddr$prim56510, align 8
%clofunc56511 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147091)
musttail call tailcc void %clofunc56511(%struct.ScmObj* %_37foldr147091, %struct.ScmObj* %argslist54550$_37foldr1470914)
ret void
}

define tailcc void @proc_clo$ae47693(%struct.ScmObj* %env$ae47693,%struct.ScmObj* %current_45args54546) {
%stackaddr$env-ref56512 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47693, i64 0)
store %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$env-ref56512
%stackaddr$env-ref56513 = alloca %struct.ScmObj*, align 8
%k47533 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47693, i64 1)
store %struct.ScmObj* %k47533, %struct.ScmObj** %stackaddr$env-ref56513
%stackaddr$env-ref56514 = alloca %struct.ScmObj*, align 8
%f47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47693, i64 2)
store %struct.ScmObj* %f47094, %struct.ScmObj** %stackaddr$env-ref56514
%stackaddr$prim56515 = alloca %struct.ScmObj*, align 8
%_95k47534 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54546)
store volatile %struct.ScmObj* %_95k47534, %struct.ScmObj** %stackaddr$prim56515, align 8
%stackaddr$prim56516 = alloca %struct.ScmObj*, align 8
%current_45args54547 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54546)
store volatile %struct.ScmObj* %current_45args54547, %struct.ScmObj** %stackaddr$prim56516, align 8
%stackaddr$prim56517 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54547)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim56517, align 8
%argslist54549$f470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56518 = alloca %struct.ScmObj*, align 8
%argslist54549$f470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47215, %struct.ScmObj* %argslist54549$f470940)
store volatile %struct.ScmObj* %argslist54549$f470941, %struct.ScmObj** %stackaddr$prim56518, align 8
%stackaddr$prim56519 = alloca %struct.ScmObj*, align 8
%argslist54549$f470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47213, %struct.ScmObj* %argslist54549$f470941)
store volatile %struct.ScmObj* %argslist54549$f470942, %struct.ScmObj** %stackaddr$prim56519, align 8
%stackaddr$prim56520 = alloca %struct.ScmObj*, align 8
%argslist54549$f470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47533, %struct.ScmObj* %argslist54549$f470942)
store volatile %struct.ScmObj* %argslist54549$f470943, %struct.ScmObj** %stackaddr$prim56520, align 8
%clofunc56521 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47094)
musttail call tailcc void %clofunc56521(%struct.ScmObj* %f47094, %struct.ScmObj* %argslist54549$f470943)
ret void
}

define tailcc void @proc_clo$ae47561(%struct.ScmObj* %env$ae47561,%struct.ScmObj* %current_45args54554) {
%stackaddr$prim56522 = alloca %struct.ScmObj*, align 8
%k47535 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54554)
store volatile %struct.ScmObj* %k47535, %struct.ScmObj** %stackaddr$prim56522, align 8
%stackaddr$prim56523 = alloca %struct.ScmObj*, align 8
%current_45args54555 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54554)
store volatile %struct.ScmObj* %current_45args54555, %struct.ScmObj** %stackaddr$prim56523, align 8
%stackaddr$prim56524 = alloca %struct.ScmObj*, align 8
%y47071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54555)
store volatile %struct.ScmObj* %y47071, %struct.ScmObj** %stackaddr$prim56524, align 8
%ae47563 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56525 = alloca %struct.ScmObj*, align 8
%fptrToInt56526 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47564 to i64
%ae47564 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56526)
store volatile %struct.ScmObj* %ae47564, %struct.ScmObj** %stackaddr$makeclosure56525, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47564, %struct.ScmObj* %y47071, i64 0)
%argslist54573$k475350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56527 = alloca %struct.ScmObj*, align 8
%argslist54573$k475351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47564, %struct.ScmObj* %argslist54573$k475350)
store volatile %struct.ScmObj* %argslist54573$k475351, %struct.ScmObj** %stackaddr$prim56527, align 8
%stackaddr$prim56528 = alloca %struct.ScmObj*, align 8
%argslist54573$k475352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47563, %struct.ScmObj* %argslist54573$k475351)
store volatile %struct.ScmObj* %argslist54573$k475352, %struct.ScmObj** %stackaddr$prim56528, align 8
%clofunc56529 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47535)
musttail call tailcc void %clofunc56529(%struct.ScmObj* %k47535, %struct.ScmObj* %argslist54573$k475352)
ret void
}

define tailcc void @proc_clo$ae47564(%struct.ScmObj* %env$ae47564,%struct.ScmObj* %current_45args54557) {
%stackaddr$env-ref56530 = alloca %struct.ScmObj*, align 8
%y47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47564, i64 0)
store %struct.ScmObj* %y47071, %struct.ScmObj** %stackaddr$env-ref56530
%stackaddr$prim56531 = alloca %struct.ScmObj*, align 8
%k47536 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54557)
store volatile %struct.ScmObj* %k47536, %struct.ScmObj** %stackaddr$prim56531, align 8
%stackaddr$prim56532 = alloca %struct.ScmObj*, align 8
%current_45args54558 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54557)
store volatile %struct.ScmObj* %current_45args54558, %struct.ScmObj** %stackaddr$prim56532, align 8
%stackaddr$prim56533 = alloca %struct.ScmObj*, align 8
%f47072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54558)
store volatile %struct.ScmObj* %f47072, %struct.ScmObj** %stackaddr$prim56533, align 8
%stackaddr$makeclosure56534 = alloca %struct.ScmObj*, align 8
%fptrToInt56535 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47565 to i64
%ae47565 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56535)
store volatile %struct.ScmObj* %ae47565, %struct.ScmObj** %stackaddr$makeclosure56534, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47565, %struct.ScmObj* %f47072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47565, %struct.ScmObj* %k47536, i64 1)
%ae47566 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56536 = alloca %struct.ScmObj*, align 8
%fptrToInt56537 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47567 to i64
%ae47567 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56537)
store volatile %struct.ScmObj* %ae47567, %struct.ScmObj** %stackaddr$makeclosure56536, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47567, %struct.ScmObj* %f47072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47567, %struct.ScmObj* %y47071, i64 1)
%argslist54572$ae475650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56538 = alloca %struct.ScmObj*, align 8
%argslist54572$ae475651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47567, %struct.ScmObj* %argslist54572$ae475650)
store volatile %struct.ScmObj* %argslist54572$ae475651, %struct.ScmObj** %stackaddr$prim56538, align 8
%stackaddr$prim56539 = alloca %struct.ScmObj*, align 8
%argslist54572$ae475652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47566, %struct.ScmObj* %argslist54572$ae475651)
store volatile %struct.ScmObj* %argslist54572$ae475652, %struct.ScmObj** %stackaddr$prim56539, align 8
%clofunc56540 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47565)
musttail call tailcc void %clofunc56540(%struct.ScmObj* %ae47565, %struct.ScmObj* %argslist54572$ae475652)
ret void
}

define tailcc void @proc_clo$ae47565(%struct.ScmObj* %env$ae47565,%struct.ScmObj* %current_45args54560) {
%stackaddr$env-ref56541 = alloca %struct.ScmObj*, align 8
%f47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47565, i64 0)
store %struct.ScmObj* %f47072, %struct.ScmObj** %stackaddr$env-ref56541
%stackaddr$env-ref56542 = alloca %struct.ScmObj*, align 8
%k47536 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47565, i64 1)
store %struct.ScmObj* %k47536, %struct.ScmObj** %stackaddr$env-ref56542
%stackaddr$prim56543 = alloca %struct.ScmObj*, align 8
%_95k47537 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54560)
store volatile %struct.ScmObj* %_95k47537, %struct.ScmObj** %stackaddr$prim56543, align 8
%stackaddr$prim56544 = alloca %struct.ScmObj*, align 8
%current_45args54561 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54560)
store volatile %struct.ScmObj* %current_45args54561, %struct.ScmObj** %stackaddr$prim56544, align 8
%stackaddr$prim56545 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54561)
store volatile %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$prim56545, align 8
%argslist54563$f470720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56546 = alloca %struct.ScmObj*, align 8
%argslist54563$f470721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47210, %struct.ScmObj* %argslist54563$f470720)
store volatile %struct.ScmObj* %argslist54563$f470721, %struct.ScmObj** %stackaddr$prim56546, align 8
%stackaddr$prim56547 = alloca %struct.ScmObj*, align 8
%argslist54563$f470722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47536, %struct.ScmObj* %argslist54563$f470721)
store volatile %struct.ScmObj* %argslist54563$f470722, %struct.ScmObj** %stackaddr$prim56547, align 8
%clofunc56548 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47072)
musttail call tailcc void %clofunc56548(%struct.ScmObj* %f47072, %struct.ScmObj* %argslist54563$f470722)
ret void
}

define tailcc void @proc_clo$ae47567(%struct.ScmObj* %env$ae47567,%struct.ScmObj* %args4707347538) {
%stackaddr$env-ref56549 = alloca %struct.ScmObj*, align 8
%f47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47567, i64 0)
store %struct.ScmObj* %f47072, %struct.ScmObj** %stackaddr$env-ref56549
%stackaddr$env-ref56550 = alloca %struct.ScmObj*, align 8
%y47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47567, i64 1)
store %struct.ScmObj* %y47071, %struct.ScmObj** %stackaddr$env-ref56550
%stackaddr$prim56551 = alloca %struct.ScmObj*, align 8
%k47539 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4707347538)
store volatile %struct.ScmObj* %k47539, %struct.ScmObj** %stackaddr$prim56551, align 8
%stackaddr$prim56552 = alloca %struct.ScmObj*, align 8
%args47073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4707347538)
store volatile %struct.ScmObj* %args47073, %struct.ScmObj** %stackaddr$prim56552, align 8
%stackaddr$makeclosure56553 = alloca %struct.ScmObj*, align 8
%fptrToInt56554 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47571 to i64
%ae47571 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56554)
store volatile %struct.ScmObj* %ae47571, %struct.ScmObj** %stackaddr$makeclosure56553, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47571, %struct.ScmObj* %k47539, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47571, %struct.ScmObj* %args47073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47571, %struct.ScmObj* %f47072, i64 2)
%argslist54571$y470710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56555 = alloca %struct.ScmObj*, align 8
%argslist54571$y470711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47071, %struct.ScmObj* %argslist54571$y470710)
store volatile %struct.ScmObj* %argslist54571$y470711, %struct.ScmObj** %stackaddr$prim56555, align 8
%stackaddr$prim56556 = alloca %struct.ScmObj*, align 8
%argslist54571$y470712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47571, %struct.ScmObj* %argslist54571$y470711)
store volatile %struct.ScmObj* %argslist54571$y470712, %struct.ScmObj** %stackaddr$prim56556, align 8
%clofunc56557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47071)
musttail call tailcc void %clofunc56557(%struct.ScmObj* %y47071, %struct.ScmObj* %argslist54571$y470712)
ret void
}

define tailcc void @proc_clo$ae47571(%struct.ScmObj* %env$ae47571,%struct.ScmObj* %current_45args54564) {
%stackaddr$env-ref56558 = alloca %struct.ScmObj*, align 8
%k47539 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47571, i64 0)
store %struct.ScmObj* %k47539, %struct.ScmObj** %stackaddr$env-ref56558
%stackaddr$env-ref56559 = alloca %struct.ScmObj*, align 8
%args47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47571, i64 1)
store %struct.ScmObj* %args47073, %struct.ScmObj** %stackaddr$env-ref56559
%stackaddr$env-ref56560 = alloca %struct.ScmObj*, align 8
%f47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47571, i64 2)
store %struct.ScmObj* %f47072, %struct.ScmObj** %stackaddr$env-ref56560
%stackaddr$prim56561 = alloca %struct.ScmObj*, align 8
%_95k47540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54564)
store volatile %struct.ScmObj* %_95k47540, %struct.ScmObj** %stackaddr$prim56561, align 8
%stackaddr$prim56562 = alloca %struct.ScmObj*, align 8
%current_45args54565 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54564)
store volatile %struct.ScmObj* %current_45args54565, %struct.ScmObj** %stackaddr$prim56562, align 8
%stackaddr$prim56563 = alloca %struct.ScmObj*, align 8
%anf_45bind47208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54565)
store volatile %struct.ScmObj* %anf_45bind47208, %struct.ScmObj** %stackaddr$prim56563, align 8
%stackaddr$makeclosure56564 = alloca %struct.ScmObj*, align 8
%fptrToInt56565 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47574 to i64
%ae47574 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56565)
store volatile %struct.ScmObj* %ae47574, %struct.ScmObj** %stackaddr$makeclosure56564, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47574, %struct.ScmObj* %k47539, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47574, %struct.ScmObj* %args47073, i64 1)
%argslist54570$anf_45bind472080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56566 = alloca %struct.ScmObj*, align 8
%argslist54570$anf_45bind472081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47072, %struct.ScmObj* %argslist54570$anf_45bind472080)
store volatile %struct.ScmObj* %argslist54570$anf_45bind472081, %struct.ScmObj** %stackaddr$prim56566, align 8
%stackaddr$prim56567 = alloca %struct.ScmObj*, align 8
%argslist54570$anf_45bind472082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47574, %struct.ScmObj* %argslist54570$anf_45bind472081)
store volatile %struct.ScmObj* %argslist54570$anf_45bind472082, %struct.ScmObj** %stackaddr$prim56567, align 8
%clofunc56568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47208)
musttail call tailcc void %clofunc56568(%struct.ScmObj* %anf_45bind47208, %struct.ScmObj* %argslist54570$anf_45bind472082)
ret void
}

define tailcc void @proc_clo$ae47574(%struct.ScmObj* %env$ae47574,%struct.ScmObj* %current_45args54567) {
%stackaddr$env-ref56569 = alloca %struct.ScmObj*, align 8
%k47539 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47574, i64 0)
store %struct.ScmObj* %k47539, %struct.ScmObj** %stackaddr$env-ref56569
%stackaddr$env-ref56570 = alloca %struct.ScmObj*, align 8
%args47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47574, i64 1)
store %struct.ScmObj* %args47073, %struct.ScmObj** %stackaddr$env-ref56570
%stackaddr$prim56571 = alloca %struct.ScmObj*, align 8
%_95k47541 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54567)
store volatile %struct.ScmObj* %_95k47541, %struct.ScmObj** %stackaddr$prim56571, align 8
%stackaddr$prim56572 = alloca %struct.ScmObj*, align 8
%current_45args54568 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54567)
store volatile %struct.ScmObj* %current_45args54568, %struct.ScmObj** %stackaddr$prim56572, align 8
%stackaddr$prim56573 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54568)
store volatile %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$prim56573, align 8
%stackaddr$prim56574 = alloca %struct.ScmObj*, align 8
%cpsargs47542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47539, %struct.ScmObj* %args47073)
store volatile %struct.ScmObj* %cpsargs47542, %struct.ScmObj** %stackaddr$prim56574, align 8
%clofunc56575 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47209)
musttail call tailcc void %clofunc56575(%struct.ScmObj* %anf_45bind47209, %struct.ScmObj* %cpsargs47542)
ret void
}

define tailcc void @proc_clo$ae47546(%struct.ScmObj* %env$ae47546,%struct.ScmObj* %current_45args54575) {
%stackaddr$prim56576 = alloca %struct.ScmObj*, align 8
%k47543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54575)
store volatile %struct.ScmObj* %k47543, %struct.ScmObj** %stackaddr$prim56576, align 8
%stackaddr$prim56577 = alloca %struct.ScmObj*, align 8
%current_45args54576 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54575)
store volatile %struct.ScmObj* %current_45args54576, %struct.ScmObj** %stackaddr$prim56577, align 8
%stackaddr$prim56578 = alloca %struct.ScmObj*, align 8
%yu47070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54576)
store volatile %struct.ScmObj* %yu47070, %struct.ScmObj** %stackaddr$prim56578, align 8
%argslist54578$yu470700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56579 = alloca %struct.ScmObj*, align 8
%argslist54578$yu470701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47070, %struct.ScmObj* %argslist54578$yu470700)
store volatile %struct.ScmObj* %argslist54578$yu470701, %struct.ScmObj** %stackaddr$prim56579, align 8
%stackaddr$prim56580 = alloca %struct.ScmObj*, align 8
%argslist54578$yu470702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47543, %struct.ScmObj* %argslist54578$yu470701)
store volatile %struct.ScmObj* %argslist54578$yu470702, %struct.ScmObj** %stackaddr$prim56580, align 8
%clofunc56581 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47070)
musttail call tailcc void %clofunc56581(%struct.ScmObj* %yu47070, %struct.ScmObj* %argslist54578$yu470702)
ret void
}