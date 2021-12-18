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

@global$sym$ae4397548646 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv47832 = call %struct.ScmObj* @const_init_null()
%mainargs47833 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv47832, %struct.ScmObj* %mainargs47833)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv47830,%struct.ScmObj* %mainargs47831) {
%stackaddr$makeclosure47834 = alloca %struct.ScmObj*, align 8
%fptrToInt47835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40587 to i64
%ae40587 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47835)
store volatile %struct.ScmObj* %ae40587, %struct.ScmObj** %stackaddr$makeclosure47834, align 8
%ae40588 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47836 = alloca %struct.ScmObj*, align 8
%fptrToInt47837 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40589 to i64
%ae40589 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47837)
store volatile %struct.ScmObj* %ae40589, %struct.ScmObj** %stackaddr$makeclosure47836, align 8
%args47829$ae40587$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47838 = alloca %struct.ScmObj*, align 8
%args47829$ae40587$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40589, %struct.ScmObj* %args47829$ae40587$0)
store volatile %struct.ScmObj* %args47829$ae40587$1, %struct.ScmObj** %stackaddr$prim47838, align 8
%stackaddr$prim47839 = alloca %struct.ScmObj*, align 8
%args47829$ae40587$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40588, %struct.ScmObj* %args47829$ae40587$1)
store volatile %struct.ScmObj* %args47829$ae40587$2, %struct.ScmObj** %stackaddr$prim47839, align 8
%clofunc47840 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40587)
musttail call tailcc void %clofunc47840(%struct.ScmObj* %ae40587, %struct.ScmObj* %args47829$ae40587$2)
ret void
}

define tailcc void @proc_clo$ae40587(%struct.ScmObj* %env$ae40587,%struct.ScmObj* %current_45args47197) {
%stackaddr$prim47841 = alloca %struct.ScmObj*, align 8
%_95k40382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47197)
store volatile %struct.ScmObj* %_95k40382, %struct.ScmObj** %stackaddr$prim47841, align 8
%stackaddr$prim47842 = alloca %struct.ScmObj*, align 8
%current_45args47198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47197)
store volatile %struct.ScmObj* %current_45args47198, %struct.ScmObj** %stackaddr$prim47842, align 8
%stackaddr$prim47843 = alloca %struct.ScmObj*, align 8
%anf_45bind40243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47198)
store volatile %struct.ScmObj* %anf_45bind40243, %struct.ScmObj** %stackaddr$prim47843, align 8
%stackaddr$makeclosure47844 = alloca %struct.ScmObj*, align 8
%fptrToInt47845 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40602 to i64
%ae40602 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47845)
store volatile %struct.ScmObj* %ae40602, %struct.ScmObj** %stackaddr$makeclosure47844, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40602, %struct.ScmObj* %anf_45bind40243, i64 0)
%ae40603 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47846 = alloca %struct.ScmObj*, align 8
%fptrToInt47847 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40604 to i64
%ae40604 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47847)
store volatile %struct.ScmObj* %ae40604, %struct.ScmObj** %stackaddr$makeclosure47846, align 8
%args47824$ae40602$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47848 = alloca %struct.ScmObj*, align 8
%args47824$ae40602$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40604, %struct.ScmObj* %args47824$ae40602$0)
store volatile %struct.ScmObj* %args47824$ae40602$1, %struct.ScmObj** %stackaddr$prim47848, align 8
%stackaddr$prim47849 = alloca %struct.ScmObj*, align 8
%args47824$ae40602$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40603, %struct.ScmObj* %args47824$ae40602$1)
store volatile %struct.ScmObj* %args47824$ae40602$2, %struct.ScmObj** %stackaddr$prim47849, align 8
%clofunc47850 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40602)
musttail call tailcc void %clofunc47850(%struct.ScmObj* %ae40602, %struct.ScmObj* %args47824$ae40602$2)
ret void
}

define tailcc void @proc_clo$ae40602(%struct.ScmObj* %env$ae40602,%struct.ScmObj* %current_45args47200) {
%stackaddr$env-ref47851 = alloca %struct.ScmObj*, align 8
%anf_45bind40243 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40602, i64 0)
store %struct.ScmObj* %anf_45bind40243, %struct.ScmObj** %stackaddr$env-ref47851
%stackaddr$prim47852 = alloca %struct.ScmObj*, align 8
%_95k40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47200)
store volatile %struct.ScmObj* %_95k40383, %struct.ScmObj** %stackaddr$prim47852, align 8
%stackaddr$prim47853 = alloca %struct.ScmObj*, align 8
%current_45args47201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47200)
store volatile %struct.ScmObj* %current_45args47201, %struct.ScmObj** %stackaddr$prim47853, align 8
%stackaddr$prim47854 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47201)
store volatile %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$prim47854, align 8
%stackaddr$makeclosure47855 = alloca %struct.ScmObj*, align 8
%fptrToInt47856 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40717 to i64
%ae40717 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47856)
store volatile %struct.ScmObj* %ae40717, %struct.ScmObj** %stackaddr$makeclosure47855, align 8
%args47803$anf_45bind40243$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47857 = alloca %struct.ScmObj*, align 8
%args47803$anf_45bind40243$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40247, %struct.ScmObj* %args47803$anf_45bind40243$0)
store volatile %struct.ScmObj* %args47803$anf_45bind40243$1, %struct.ScmObj** %stackaddr$prim47857, align 8
%stackaddr$prim47858 = alloca %struct.ScmObj*, align 8
%args47803$anf_45bind40243$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40717, %struct.ScmObj* %args47803$anf_45bind40243$1)
store volatile %struct.ScmObj* %args47803$anf_45bind40243$2, %struct.ScmObj** %stackaddr$prim47858, align 8
%clofunc47859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40243)
musttail call tailcc void %clofunc47859(%struct.ScmObj* %anf_45bind40243, %struct.ScmObj* %args47803$anf_45bind40243$2)
ret void
}

define tailcc void @proc_clo$ae40717(%struct.ScmObj* %env$ae40717,%struct.ScmObj* %current_45args47203) {
%stackaddr$prim47860 = alloca %struct.ScmObj*, align 8
%_95k40384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47203)
store volatile %struct.ScmObj* %_95k40384, %struct.ScmObj** %stackaddr$prim47860, align 8
%stackaddr$prim47861 = alloca %struct.ScmObj*, align 8
%current_45args47204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47203)
store volatile %struct.ScmObj* %current_45args47204, %struct.ScmObj** %stackaddr$prim47861, align 8
%stackaddr$prim47862 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47204)
store volatile %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$prim47862, align 8
%stackaddr$makeclosure47863 = alloca %struct.ScmObj*, align 8
%fptrToInt47864 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40719 to i64
%ae40719 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47864)
store volatile %struct.ScmObj* %ae40719, %struct.ScmObj** %stackaddr$makeclosure47863, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40719, %struct.ScmObj* %Ycmb40103, i64 0)
%ae40720 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47865 = alloca %struct.ScmObj*, align 8
%fptrToInt47866 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40721 to i64
%ae40721 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47866)
store volatile %struct.ScmObj* %ae40721, %struct.ScmObj** %stackaddr$makeclosure47865, align 8
%args47802$ae40719$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47867 = alloca %struct.ScmObj*, align 8
%args47802$ae40719$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40721, %struct.ScmObj* %args47802$ae40719$0)
store volatile %struct.ScmObj* %args47802$ae40719$1, %struct.ScmObj** %stackaddr$prim47867, align 8
%stackaddr$prim47868 = alloca %struct.ScmObj*, align 8
%args47802$ae40719$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40720, %struct.ScmObj* %args47802$ae40719$1)
store volatile %struct.ScmObj* %args47802$ae40719$2, %struct.ScmObj** %stackaddr$prim47868, align 8
%clofunc47869 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40719)
musttail call tailcc void %clofunc47869(%struct.ScmObj* %ae40719, %struct.ScmObj* %args47802$ae40719$2)
ret void
}

define tailcc void @proc_clo$ae40719(%struct.ScmObj* %env$ae40719,%struct.ScmObj* %current_45args47206) {
%stackaddr$env-ref47870 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40719, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47870
%stackaddr$prim47871 = alloca %struct.ScmObj*, align 8
%_95k40385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47206)
store volatile %struct.ScmObj* %_95k40385, %struct.ScmObj** %stackaddr$prim47871, align 8
%stackaddr$prim47872 = alloca %struct.ScmObj*, align 8
%current_45args47207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47206)
store volatile %struct.ScmObj* %current_45args47207, %struct.ScmObj** %stackaddr$prim47872, align 8
%stackaddr$prim47873 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47207)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim47873, align 8
%stackaddr$makeclosure47874 = alloca %struct.ScmObj*, align 8
%fptrToInt47875 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40797 to i64
%ae40797 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47875)
store volatile %struct.ScmObj* %ae40797, %struct.ScmObj** %stackaddr$makeclosure47874, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40797, %struct.ScmObj* %Ycmb40103, i64 0)
%args47786$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47876 = alloca %struct.ScmObj*, align 8
%args47786$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40252, %struct.ScmObj* %args47786$Ycmb40103$0)
store volatile %struct.ScmObj* %args47786$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim47876, align 8
%stackaddr$prim47877 = alloca %struct.ScmObj*, align 8
%args47786$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40797, %struct.ScmObj* %args47786$Ycmb40103$1)
store volatile %struct.ScmObj* %args47786$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim47877, align 8
%clofunc47878 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc47878(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args47786$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae40797(%struct.ScmObj* %env$ae40797,%struct.ScmObj* %current_45args47209) {
%stackaddr$env-ref47879 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40797, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47879
%stackaddr$prim47880 = alloca %struct.ScmObj*, align 8
%_95k40386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47209)
store volatile %struct.ScmObj* %_95k40386, %struct.ScmObj** %stackaddr$prim47880, align 8
%stackaddr$prim47881 = alloca %struct.ScmObj*, align 8
%current_45args47210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47209)
store volatile %struct.ScmObj* %current_45args47210, %struct.ScmObj** %stackaddr$prim47881, align 8
%stackaddr$prim47882 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47210)
store volatile %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$prim47882, align 8
%stackaddr$makeclosure47883 = alloca %struct.ScmObj*, align 8
%fptrToInt47884 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40799 to i64
%ae40799 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47884)
store volatile %struct.ScmObj* %ae40799, %struct.ScmObj** %stackaddr$makeclosure47883, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40799, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40799, %struct.ScmObj* %_37foldr140124, i64 1)
%ae40800 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47885 = alloca %struct.ScmObj*, align 8
%fptrToInt47886 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40801 to i64
%ae40801 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47886)
store volatile %struct.ScmObj* %ae40801, %struct.ScmObj** %stackaddr$makeclosure47885, align 8
%args47785$ae40799$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47887 = alloca %struct.ScmObj*, align 8
%args47785$ae40799$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40801, %struct.ScmObj* %args47785$ae40799$0)
store volatile %struct.ScmObj* %args47785$ae40799$1, %struct.ScmObj** %stackaddr$prim47887, align 8
%stackaddr$prim47888 = alloca %struct.ScmObj*, align 8
%args47785$ae40799$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40800, %struct.ScmObj* %args47785$ae40799$1)
store volatile %struct.ScmObj* %args47785$ae40799$2, %struct.ScmObj** %stackaddr$prim47888, align 8
%clofunc47889 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40799)
musttail call tailcc void %clofunc47889(%struct.ScmObj* %ae40799, %struct.ScmObj* %args47785$ae40799$2)
ret void
}

define tailcc void @proc_clo$ae40799(%struct.ScmObj* %env$ae40799,%struct.ScmObj* %current_45args47212) {
%stackaddr$env-ref47890 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40799, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47890
%stackaddr$env-ref47891 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40799, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47891
%stackaddr$prim47892 = alloca %struct.ScmObj*, align 8
%_95k40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47212)
store volatile %struct.ScmObj* %_95k40387, %struct.ScmObj** %stackaddr$prim47892, align 8
%stackaddr$prim47893 = alloca %struct.ScmObj*, align 8
%current_45args47213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47212)
store volatile %struct.ScmObj* %current_45args47213, %struct.ScmObj** %stackaddr$prim47893, align 8
%stackaddr$prim47894 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47213)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim47894, align 8
%stackaddr$makeclosure47895 = alloca %struct.ScmObj*, align 8
%fptrToInt47896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40894 to i64
%ae40894 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47896)
store volatile %struct.ScmObj* %ae40894, %struct.ScmObj** %stackaddr$makeclosure47895, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40894, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40894, %struct.ScmObj* %_37foldr140124, i64 1)
%args47766$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47897 = alloca %struct.ScmObj*, align 8
%args47766$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40258, %struct.ScmObj* %args47766$Ycmb40103$0)
store volatile %struct.ScmObj* %args47766$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim47897, align 8
%stackaddr$prim47898 = alloca %struct.ScmObj*, align 8
%args47766$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40894, %struct.ScmObj* %args47766$Ycmb40103$1)
store volatile %struct.ScmObj* %args47766$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim47898, align 8
%clofunc47899 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc47899(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args47766$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae40894(%struct.ScmObj* %env$ae40894,%struct.ScmObj* %current_45args47215) {
%stackaddr$env-ref47900 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40894, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47900
%stackaddr$env-ref47901 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40894, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47901
%stackaddr$prim47902 = alloca %struct.ScmObj*, align 8
%_95k40388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47215)
store volatile %struct.ScmObj* %_95k40388, %struct.ScmObj** %stackaddr$prim47902, align 8
%stackaddr$prim47903 = alloca %struct.ScmObj*, align 8
%current_45args47216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47215)
store volatile %struct.ScmObj* %current_45args47216, %struct.ScmObj** %stackaddr$prim47903, align 8
%stackaddr$prim47904 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47216)
store volatile %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$prim47904, align 8
%stackaddr$makeclosure47905 = alloca %struct.ScmObj*, align 8
%fptrToInt47906 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40896 to i64
%ae40896 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47906)
store volatile %struct.ScmObj* %ae40896, %struct.ScmObj** %stackaddr$makeclosure47905, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40896, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40896, %struct.ScmObj* %_37foldr140124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40896, %struct.ScmObj* %_37map140120, i64 2)
%ae40897 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47907 = alloca %struct.ScmObj*, align 8
%fptrToInt47908 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40898 to i64
%ae40898 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47908)
store volatile %struct.ScmObj* %ae40898, %struct.ScmObj** %stackaddr$makeclosure47907, align 8
%args47765$ae40896$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47909 = alloca %struct.ScmObj*, align 8
%args47765$ae40896$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40898, %struct.ScmObj* %args47765$ae40896$0)
store volatile %struct.ScmObj* %args47765$ae40896$1, %struct.ScmObj** %stackaddr$prim47909, align 8
%stackaddr$prim47910 = alloca %struct.ScmObj*, align 8
%args47765$ae40896$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40897, %struct.ScmObj* %args47765$ae40896$1)
store volatile %struct.ScmObj* %args47765$ae40896$2, %struct.ScmObj** %stackaddr$prim47910, align 8
%clofunc47911 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40896)
musttail call tailcc void %clofunc47911(%struct.ScmObj* %ae40896, %struct.ScmObj* %args47765$ae40896$2)
ret void
}

define tailcc void @proc_clo$ae40896(%struct.ScmObj* %env$ae40896,%struct.ScmObj* %current_45args47218) {
%stackaddr$env-ref47912 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40896, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47912
%stackaddr$env-ref47913 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40896, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47913
%stackaddr$env-ref47914 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40896, i64 2)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47914
%stackaddr$prim47915 = alloca %struct.ScmObj*, align 8
%_95k40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47218)
store volatile %struct.ScmObj* %_95k40389, %struct.ScmObj** %stackaddr$prim47915, align 8
%stackaddr$prim47916 = alloca %struct.ScmObj*, align 8
%current_45args47219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47218)
store volatile %struct.ScmObj* %current_45args47219, %struct.ScmObj** %stackaddr$prim47916, align 8
%stackaddr$prim47917 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47219)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim47917, align 8
%stackaddr$makeclosure47918 = alloca %struct.ScmObj*, align 8
%fptrToInt47919 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41044 to i64
%ae41044 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47919)
store volatile %struct.ScmObj* %ae41044, %struct.ScmObj** %stackaddr$makeclosure47918, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41044, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41044, %struct.ScmObj* %_37foldr140124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41044, %struct.ScmObj* %_37map140120, i64 2)
%args47749$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47920 = alloca %struct.ScmObj*, align 8
%args47749$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40265, %struct.ScmObj* %args47749$Ycmb40103$0)
store volatile %struct.ScmObj* %args47749$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim47920, align 8
%stackaddr$prim47921 = alloca %struct.ScmObj*, align 8
%args47749$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41044, %struct.ScmObj* %args47749$Ycmb40103$1)
store volatile %struct.ScmObj* %args47749$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim47921, align 8
%clofunc47922 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc47922(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args47749$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae41044(%struct.ScmObj* %env$ae41044,%struct.ScmObj* %current_45args47221) {
%stackaddr$env-ref47923 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41044, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47923
%stackaddr$env-ref47924 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41044, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47924
%stackaddr$env-ref47925 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41044, i64 2)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47925
%stackaddr$prim47926 = alloca %struct.ScmObj*, align 8
%_95k40390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47221)
store volatile %struct.ScmObj* %_95k40390, %struct.ScmObj** %stackaddr$prim47926, align 8
%stackaddr$prim47927 = alloca %struct.ScmObj*, align 8
%current_45args47222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47221)
store volatile %struct.ScmObj* %current_45args47222, %struct.ScmObj** %stackaddr$prim47927, align 8
%stackaddr$prim47928 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47222)
store volatile %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$prim47928, align 8
%stackaddr$makeclosure47929 = alloca %struct.ScmObj*, align 8
%fptrToInt47930 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41046 to i64
%ae41046 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47930)
store volatile %struct.ScmObj* %ae41046, %struct.ScmObj** %stackaddr$makeclosure47929, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41046, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41046, %struct.ScmObj* %_37take40116, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41046, %struct.ScmObj* %_37foldr140124, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41046, %struct.ScmObj* %_37map140120, i64 3)
%ae41047 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47931 = alloca %struct.ScmObj*, align 8
%fptrToInt47932 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41048 to i64
%ae41048 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47932)
store volatile %struct.ScmObj* %ae41048, %struct.ScmObj** %stackaddr$makeclosure47931, align 8
%args47748$ae41046$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47933 = alloca %struct.ScmObj*, align 8
%args47748$ae41046$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41048, %struct.ScmObj* %args47748$ae41046$0)
store volatile %struct.ScmObj* %args47748$ae41046$1, %struct.ScmObj** %stackaddr$prim47933, align 8
%stackaddr$prim47934 = alloca %struct.ScmObj*, align 8
%args47748$ae41046$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41047, %struct.ScmObj* %args47748$ae41046$1)
store volatile %struct.ScmObj* %args47748$ae41046$2, %struct.ScmObj** %stackaddr$prim47934, align 8
%clofunc47935 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41046)
musttail call tailcc void %clofunc47935(%struct.ScmObj* %ae41046, %struct.ScmObj* %args47748$ae41046$2)
ret void
}

define tailcc void @proc_clo$ae41046(%struct.ScmObj* %env$ae41046,%struct.ScmObj* %current_45args47224) {
%stackaddr$env-ref47936 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41046, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47936
%stackaddr$env-ref47937 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41046, i64 1)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref47937
%stackaddr$env-ref47938 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41046, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47938
%stackaddr$env-ref47939 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41046, i64 3)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47939
%stackaddr$prim47940 = alloca %struct.ScmObj*, align 8
%_95k40391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47224)
store volatile %struct.ScmObj* %_95k40391, %struct.ScmObj** %stackaddr$prim47940, align 8
%stackaddr$prim47941 = alloca %struct.ScmObj*, align 8
%current_45args47225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47224)
store volatile %struct.ScmObj* %current_45args47225, %struct.ScmObj** %stackaddr$prim47941, align 8
%stackaddr$prim47942 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47225)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim47942, align 8
%stackaddr$makeclosure47943 = alloca %struct.ScmObj*, align 8
%fptrToInt47944 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41127 to i64
%ae41127 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47944)
store volatile %struct.ScmObj* %ae41127, %struct.ScmObj** %stackaddr$makeclosure47943, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41127, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41127, %struct.ScmObj* %_37take40116, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41127, %struct.ScmObj* %_37foldr140124, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41127, %struct.ScmObj* %_37map140120, i64 3)
%args47734$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47945 = alloca %struct.ScmObj*, align 8
%args47734$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40269, %struct.ScmObj* %args47734$Ycmb40103$0)
store volatile %struct.ScmObj* %args47734$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim47945, align 8
%stackaddr$prim47946 = alloca %struct.ScmObj*, align 8
%args47734$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41127, %struct.ScmObj* %args47734$Ycmb40103$1)
store volatile %struct.ScmObj* %args47734$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim47946, align 8
%clofunc47947 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc47947(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args47734$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae41127(%struct.ScmObj* %env$ae41127,%struct.ScmObj* %current_45args47227) {
%stackaddr$env-ref47948 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41127, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47948
%stackaddr$env-ref47949 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41127, i64 1)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref47949
%stackaddr$env-ref47950 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41127, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47950
%stackaddr$env-ref47951 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41127, i64 3)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47951
%stackaddr$prim47952 = alloca %struct.ScmObj*, align 8
%_95k40392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47227)
store volatile %struct.ScmObj* %_95k40392, %struct.ScmObj** %stackaddr$prim47952, align 8
%stackaddr$prim47953 = alloca %struct.ScmObj*, align 8
%current_45args47228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47227)
store volatile %struct.ScmObj* %current_45args47228, %struct.ScmObj** %stackaddr$prim47953, align 8
%stackaddr$prim47954 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47228)
store volatile %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$prim47954, align 8
%stackaddr$makeclosure47955 = alloca %struct.ScmObj*, align 8
%fptrToInt47956 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41129 to i64
%ae41129 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47956)
store volatile %struct.ScmObj* %ae41129, %struct.ScmObj** %stackaddr$makeclosure47955, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %_37take40116, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %_37length40113, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %_37foldr140124, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %_37map140120, i64 4)
%ae41130 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47957 = alloca %struct.ScmObj*, align 8
%fptrToInt47958 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41131 to i64
%ae41131 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47958)
store volatile %struct.ScmObj* %ae41131, %struct.ScmObj** %stackaddr$makeclosure47957, align 8
%args47733$ae41129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47959 = alloca %struct.ScmObj*, align 8
%args47733$ae41129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41131, %struct.ScmObj* %args47733$ae41129$0)
store volatile %struct.ScmObj* %args47733$ae41129$1, %struct.ScmObj** %stackaddr$prim47959, align 8
%stackaddr$prim47960 = alloca %struct.ScmObj*, align 8
%args47733$ae41129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41130, %struct.ScmObj* %args47733$ae41129$1)
store volatile %struct.ScmObj* %args47733$ae41129$2, %struct.ScmObj** %stackaddr$prim47960, align 8
%clofunc47961 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41129)
musttail call tailcc void %clofunc47961(%struct.ScmObj* %ae41129, %struct.ScmObj* %args47733$ae41129$2)
ret void
}

define tailcc void @proc_clo$ae41129(%struct.ScmObj* %env$ae41129,%struct.ScmObj* %current_45args47230) {
%stackaddr$env-ref47962 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47962
%stackaddr$env-ref47963 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 1)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref47963
%stackaddr$env-ref47964 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 2)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref47964
%stackaddr$env-ref47965 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47965
%stackaddr$env-ref47966 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 4)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47966
%stackaddr$prim47967 = alloca %struct.ScmObj*, align 8
%_95k40393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47230)
store volatile %struct.ScmObj* %_95k40393, %struct.ScmObj** %stackaddr$prim47967, align 8
%stackaddr$prim47968 = alloca %struct.ScmObj*, align 8
%current_45args47231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47230)
store volatile %struct.ScmObj* %current_45args47231, %struct.ScmObj** %stackaddr$prim47968, align 8
%stackaddr$prim47969 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47231)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim47969, align 8
%stackaddr$makeclosure47970 = alloca %struct.ScmObj*, align 8
%fptrToInt47971 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41206 to i64
%ae41206 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47971)
store volatile %struct.ScmObj* %ae41206, %struct.ScmObj** %stackaddr$makeclosure47970, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41206, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41206, %struct.ScmObj* %_37take40116, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41206, %struct.ScmObj* %_37length40113, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41206, %struct.ScmObj* %_37foldr140124, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41206, %struct.ScmObj* %_37map140120, i64 4)
%args47717$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47972 = alloca %struct.ScmObj*, align 8
%args47717$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40274, %struct.ScmObj* %args47717$Ycmb40103$0)
store volatile %struct.ScmObj* %args47717$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim47972, align 8
%stackaddr$prim47973 = alloca %struct.ScmObj*, align 8
%args47717$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41206, %struct.ScmObj* %args47717$Ycmb40103$1)
store volatile %struct.ScmObj* %args47717$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim47973, align 8
%clofunc47974 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc47974(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args47717$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae41206(%struct.ScmObj* %env$ae41206,%struct.ScmObj* %current_45args47233) {
%stackaddr$env-ref47975 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41206, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47975
%stackaddr$env-ref47976 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41206, i64 1)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref47976
%stackaddr$env-ref47977 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41206, i64 2)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref47977
%stackaddr$env-ref47978 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41206, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47978
%stackaddr$env-ref47979 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41206, i64 4)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47979
%stackaddr$prim47980 = alloca %struct.ScmObj*, align 8
%_95k40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47233)
store volatile %struct.ScmObj* %_95k40394, %struct.ScmObj** %stackaddr$prim47980, align 8
%stackaddr$prim47981 = alloca %struct.ScmObj*, align 8
%current_45args47234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47233)
store volatile %struct.ScmObj* %current_45args47234, %struct.ScmObj** %stackaddr$prim47981, align 8
%stackaddr$prim47982 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47234)
store volatile %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$prim47982, align 8
%stackaddr$makeclosure47983 = alloca %struct.ScmObj*, align 8
%fptrToInt47984 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41208 to i64
%ae41208 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47984)
store volatile %struct.ScmObj* %ae41208, %struct.ScmObj** %stackaddr$makeclosure47983, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41208, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41208, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41208, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41208, %struct.ScmObj* %_37take40116, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41208, %struct.ScmObj* %_37length40113, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41208, %struct.ScmObj* %_37map140120, i64 5)
%ae41209 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47985 = alloca %struct.ScmObj*, align 8
%fptrToInt47986 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41210 to i64
%ae41210 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47986)
store volatile %struct.ScmObj* %ae41210, %struct.ScmObj** %stackaddr$makeclosure47985, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41210, %struct.ScmObj* %_37foldl140108, i64 0)
%args47716$ae41208$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47987 = alloca %struct.ScmObj*, align 8
%args47716$ae41208$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41210, %struct.ScmObj* %args47716$ae41208$0)
store volatile %struct.ScmObj* %args47716$ae41208$1, %struct.ScmObj** %stackaddr$prim47987, align 8
%stackaddr$prim47988 = alloca %struct.ScmObj*, align 8
%args47716$ae41208$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41209, %struct.ScmObj* %args47716$ae41208$1)
store volatile %struct.ScmObj* %args47716$ae41208$2, %struct.ScmObj** %stackaddr$prim47988, align 8
%clofunc47989 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41208)
musttail call tailcc void %clofunc47989(%struct.ScmObj* %ae41208, %struct.ScmObj* %args47716$ae41208$2)
ret void
}

define tailcc void @proc_clo$ae41208(%struct.ScmObj* %env$ae41208,%struct.ScmObj* %current_45args47236) {
%stackaddr$env-ref47990 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41208, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47990
%stackaddr$env-ref47991 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41208, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47991
%stackaddr$env-ref47992 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41208, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47992
%stackaddr$env-ref47993 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41208, i64 3)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref47993
%stackaddr$env-ref47994 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41208, i64 4)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref47994
%stackaddr$env-ref47995 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41208, i64 5)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47995
%stackaddr$prim47996 = alloca %struct.ScmObj*, align 8
%_95k40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47236)
store volatile %struct.ScmObj* %_95k40395, %struct.ScmObj** %stackaddr$prim47996, align 8
%stackaddr$prim47997 = alloca %struct.ScmObj*, align 8
%current_45args47237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47236)
store volatile %struct.ScmObj* %current_45args47237, %struct.ScmObj** %stackaddr$prim47997, align 8
%stackaddr$prim47998 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47237)
store volatile %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$prim47998, align 8
%stackaddr$makeclosure47999 = alloca %struct.ScmObj*, align 8
%fptrToInt48000 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41262 to i64
%ae41262 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48000)
store volatile %struct.ScmObj* %ae41262, %struct.ScmObj** %stackaddr$makeclosure47999, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41262, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41262, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41262, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41262, %struct.ScmObj* %_37last40146, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41262, %struct.ScmObj* %_37map140120, i64 4)
%ae41263 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48001 = alloca %struct.ScmObj*, align 8
%fptrToInt48002 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41264 to i64
%ae41264 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48002)
store volatile %struct.ScmObj* %ae41264, %struct.ScmObj** %stackaddr$makeclosure48001, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41264, %struct.ScmObj* %_37take40116, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41264, %struct.ScmObj* %_37length40113, i64 1)
%args47702$ae41262$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48003 = alloca %struct.ScmObj*, align 8
%args47702$ae41262$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41264, %struct.ScmObj* %args47702$ae41262$0)
store volatile %struct.ScmObj* %args47702$ae41262$1, %struct.ScmObj** %stackaddr$prim48003, align 8
%stackaddr$prim48004 = alloca %struct.ScmObj*, align 8
%args47702$ae41262$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41263, %struct.ScmObj* %args47702$ae41262$1)
store volatile %struct.ScmObj* %args47702$ae41262$2, %struct.ScmObj** %stackaddr$prim48004, align 8
%clofunc48005 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41262)
musttail call tailcc void %clofunc48005(%struct.ScmObj* %ae41262, %struct.ScmObj* %args47702$ae41262$2)
ret void
}

define tailcc void @proc_clo$ae41262(%struct.ScmObj* %env$ae41262,%struct.ScmObj* %current_45args47239) {
%stackaddr$env-ref48006 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41262, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48006
%stackaddr$env-ref48007 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41262, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48007
%stackaddr$env-ref48008 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41262, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref48008
%stackaddr$env-ref48009 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41262, i64 3)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref48009
%stackaddr$env-ref48010 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41262, i64 4)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref48010
%stackaddr$prim48011 = alloca %struct.ScmObj*, align 8
%_95k40396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47239)
store volatile %struct.ScmObj* %_95k40396, %struct.ScmObj** %stackaddr$prim48011, align 8
%stackaddr$prim48012 = alloca %struct.ScmObj*, align 8
%current_45args47240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47239)
store volatile %struct.ScmObj* %current_45args47240, %struct.ScmObj** %stackaddr$prim48012, align 8
%stackaddr$prim48013 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47240)
store volatile %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$prim48013, align 8
%stackaddr$makeclosure48014 = alloca %struct.ScmObj*, align 8
%fptrToInt48015 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41292 to i64
%ae41292 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48015)
store volatile %struct.ScmObj* %ae41292, %struct.ScmObj** %stackaddr$makeclosure48014, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41292, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41292, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41292, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41292, %struct.ScmObj* %_37last40146, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41292, %struct.ScmObj* %_37drop_45right40143, i64 4)
%ae41293 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48016 = alloca %struct.ScmObj*, align 8
%fptrToInt48017 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41294 to i64
%ae41294 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48017)
store volatile %struct.ScmObj* %ae41294, %struct.ScmObj** %stackaddr$makeclosure48016, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %_37map140120, i64 1)
%args47692$ae41292$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48018 = alloca %struct.ScmObj*, align 8
%args47692$ae41292$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41294, %struct.ScmObj* %args47692$ae41292$0)
store volatile %struct.ScmObj* %args47692$ae41292$1, %struct.ScmObj** %stackaddr$prim48018, align 8
%stackaddr$prim48019 = alloca %struct.ScmObj*, align 8
%args47692$ae41292$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41293, %struct.ScmObj* %args47692$ae41292$1)
store volatile %struct.ScmObj* %args47692$ae41292$2, %struct.ScmObj** %stackaddr$prim48019, align 8
%clofunc48020 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41292)
musttail call tailcc void %clofunc48020(%struct.ScmObj* %ae41292, %struct.ScmObj* %args47692$ae41292$2)
ret void
}

define tailcc void @proc_clo$ae41292(%struct.ScmObj* %env$ae41292,%struct.ScmObj* %current_45args47242) {
%stackaddr$env-ref48021 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41292, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48021
%stackaddr$env-ref48022 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41292, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48022
%stackaddr$env-ref48023 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41292, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref48023
%stackaddr$env-ref48024 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41292, i64 3)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref48024
%stackaddr$env-ref48025 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41292, i64 4)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref48025
%stackaddr$prim48026 = alloca %struct.ScmObj*, align 8
%_95k40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47242)
store volatile %struct.ScmObj* %_95k40397, %struct.ScmObj** %stackaddr$prim48026, align 8
%stackaddr$prim48027 = alloca %struct.ScmObj*, align 8
%current_45args47243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47242)
store volatile %struct.ScmObj* %current_45args47243, %struct.ScmObj** %stackaddr$prim48027, align 8
%stackaddr$prim48028 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47243)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim48028, align 8
%stackaddr$makeclosure48029 = alloca %struct.ScmObj*, align 8
%fptrToInt48030 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41676 to i64
%ae41676 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48030)
store volatile %struct.ScmObj* %ae41676, %struct.ScmObj** %stackaddr$makeclosure48029, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41676, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41676, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41676, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41676, %struct.ScmObj* %_37last40146, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41676, %struct.ScmObj* %_37drop_45right40143, i64 4)
%args47632$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48031 = alloca %struct.ScmObj*, align 8
%args47632$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40290, %struct.ScmObj* %args47632$Ycmb40103$0)
store volatile %struct.ScmObj* %args47632$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim48031, align 8
%stackaddr$prim48032 = alloca %struct.ScmObj*, align 8
%args47632$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41676, %struct.ScmObj* %args47632$Ycmb40103$1)
store volatile %struct.ScmObj* %args47632$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim48032, align 8
%clofunc48033 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc48033(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args47632$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae41676(%struct.ScmObj* %env$ae41676,%struct.ScmObj* %current_45args47245) {
%stackaddr$env-ref48034 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41676, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48034
%stackaddr$env-ref48035 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41676, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48035
%stackaddr$env-ref48036 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41676, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref48036
%stackaddr$env-ref48037 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41676, i64 3)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref48037
%stackaddr$env-ref48038 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41676, i64 4)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref48038
%stackaddr$prim48039 = alloca %struct.ScmObj*, align 8
%_95k40398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47245)
store volatile %struct.ScmObj* %_95k40398, %struct.ScmObj** %stackaddr$prim48039, align 8
%stackaddr$prim48040 = alloca %struct.ScmObj*, align 8
%current_45args47246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47245)
store volatile %struct.ScmObj* %current_45args47246, %struct.ScmObj** %stackaddr$prim48040, align 8
%stackaddr$prim48041 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47246)
store volatile %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$prim48041, align 8
%stackaddr$makeclosure48042 = alloca %struct.ScmObj*, align 8
%fptrToInt48043 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41678 to i64
%ae41678 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48043)
store volatile %struct.ScmObj* %ae41678, %struct.ScmObj** %stackaddr$makeclosure48042, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41678, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41678, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41678, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41678, %struct.ScmObj* %_37last40146, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41678, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41678, %struct.ScmObj* %_37drop_45right40143, i64 5)
%ae41679 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48044 = alloca %struct.ScmObj*, align 8
%fptrToInt48045 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41680 to i64
%ae41680 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48045)
store volatile %struct.ScmObj* %ae41680, %struct.ScmObj** %stackaddr$makeclosure48044, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41680, %struct.ScmObj* %_37foldr140124, i64 0)
%args47631$ae41678$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48046 = alloca %struct.ScmObj*, align 8
%args47631$ae41678$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41680, %struct.ScmObj* %args47631$ae41678$0)
store volatile %struct.ScmObj* %args47631$ae41678$1, %struct.ScmObj** %stackaddr$prim48046, align 8
%stackaddr$prim48047 = alloca %struct.ScmObj*, align 8
%args47631$ae41678$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41679, %struct.ScmObj* %args47631$ae41678$1)
store volatile %struct.ScmObj* %args47631$ae41678$2, %struct.ScmObj** %stackaddr$prim48047, align 8
%clofunc48048 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41678)
musttail call tailcc void %clofunc48048(%struct.ScmObj* %ae41678, %struct.ScmObj* %args47631$ae41678$2)
ret void
}

define tailcc void @proc_clo$ae41678(%struct.ScmObj* %env$ae41678,%struct.ScmObj* %current_45args47248) {
%stackaddr$env-ref48049 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41678, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48049
%stackaddr$env-ref48050 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41678, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48050
%stackaddr$env-ref48051 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41678, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref48051
%stackaddr$env-ref48052 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41678, i64 3)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref48052
%stackaddr$env-ref48053 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41678, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48053
%stackaddr$env-ref48054 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41678, i64 5)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref48054
%stackaddr$prim48055 = alloca %struct.ScmObj*, align 8
%_95k40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47248)
store volatile %struct.ScmObj* %_95k40399, %struct.ScmObj** %stackaddr$prim48055, align 8
%stackaddr$prim48056 = alloca %struct.ScmObj*, align 8
%current_45args47249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47248)
store volatile %struct.ScmObj* %current_45args47249, %struct.ScmObj** %stackaddr$prim48056, align 8
%stackaddr$prim48057 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47249)
store volatile %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$prim48057, align 8
%stackaddr$makeclosure48058 = alloca %struct.ScmObj*, align 8
%fptrToInt48059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41755 to i64
%ae41755 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48059)
store volatile %struct.ScmObj* %ae41755, %struct.ScmObj** %stackaddr$makeclosure48058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41755, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41755, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41755, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41755, %struct.ScmObj* %_37foldr40129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41755, %struct.ScmObj* %_37map140155, i64 4)
%ae41756 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48060 = alloca %struct.ScmObj*, align 8
%fptrToInt48061 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41757 to i64
%ae41757 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48061)
store volatile %struct.ScmObj* %ae41757, %struct.ScmObj** %stackaddr$makeclosure48060, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41757, %struct.ScmObj* %_37last40146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41757, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41757, %struct.ScmObj* %_37drop_45right40143, i64 2)
%args47612$ae41755$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48062 = alloca %struct.ScmObj*, align 8
%args47612$ae41755$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41757, %struct.ScmObj* %args47612$ae41755$0)
store volatile %struct.ScmObj* %args47612$ae41755$1, %struct.ScmObj** %stackaddr$prim48062, align 8
%stackaddr$prim48063 = alloca %struct.ScmObj*, align 8
%args47612$ae41755$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41756, %struct.ScmObj* %args47612$ae41755$1)
store volatile %struct.ScmObj* %args47612$ae41755$2, %struct.ScmObj** %stackaddr$prim48063, align 8
%clofunc48064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41755)
musttail call tailcc void %clofunc48064(%struct.ScmObj* %ae41755, %struct.ScmObj* %args47612$ae41755$2)
ret void
}

define tailcc void @proc_clo$ae41755(%struct.ScmObj* %env$ae41755,%struct.ScmObj* %current_45args47251) {
%stackaddr$env-ref48065 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41755, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48065
%stackaddr$env-ref48066 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41755, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48066
%stackaddr$env-ref48067 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41755, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref48067
%stackaddr$env-ref48068 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41755, i64 3)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48068
%stackaddr$env-ref48069 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41755, i64 4)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref48069
%stackaddr$prim48070 = alloca %struct.ScmObj*, align 8
%_95k40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47251)
store volatile %struct.ScmObj* %_95k40400, %struct.ScmObj** %stackaddr$prim48070, align 8
%stackaddr$prim48071 = alloca %struct.ScmObj*, align 8
%current_45args47252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47251)
store volatile %struct.ScmObj* %current_45args47252, %struct.ScmObj** %stackaddr$prim48071, align 8
%stackaddr$prim48072 = alloca %struct.ScmObj*, align 8
%_37map40150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47252)
store volatile %struct.ScmObj* %_37map40150, %struct.ScmObj** %stackaddr$prim48072, align 8
%stackaddr$makeclosure48073 = alloca %struct.ScmObj*, align 8
%fptrToInt48074 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41901 to i64
%ae41901 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48074)
store volatile %struct.ScmObj* %ae41901, %struct.ScmObj** %stackaddr$makeclosure48073, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41901, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41901, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41901, %struct.ScmObj* %Ycmb40103, i64 2)
%ae41902 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48075 = alloca %struct.ScmObj*, align 8
%fptrToInt48076 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41903 to i64
%ae41903 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48076)
store volatile %struct.ScmObj* %ae41903, %struct.ScmObj** %stackaddr$makeclosure48075, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %_37foldr40129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %_37foldr140124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %_37map140155, i64 2)
%args47595$ae41901$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48077 = alloca %struct.ScmObj*, align 8
%args47595$ae41901$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41903, %struct.ScmObj* %args47595$ae41901$0)
store volatile %struct.ScmObj* %args47595$ae41901$1, %struct.ScmObj** %stackaddr$prim48077, align 8
%stackaddr$prim48078 = alloca %struct.ScmObj*, align 8
%args47595$ae41901$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41902, %struct.ScmObj* %args47595$ae41901$1)
store volatile %struct.ScmObj* %args47595$ae41901$2, %struct.ScmObj** %stackaddr$prim48078, align 8
%clofunc48079 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41901)
musttail call tailcc void %clofunc48079(%struct.ScmObj* %ae41901, %struct.ScmObj* %args47595$ae41901$2)
ret void
}

define tailcc void @proc_clo$ae41901(%struct.ScmObj* %env$ae41901,%struct.ScmObj* %current_45args47254) {
%stackaddr$env-ref48080 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41901, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48080
%stackaddr$env-ref48081 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41901, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48081
%stackaddr$env-ref48082 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41901, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref48082
%stackaddr$prim48083 = alloca %struct.ScmObj*, align 8
%_95k40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47254)
store volatile %struct.ScmObj* %_95k40401, %struct.ScmObj** %stackaddr$prim48083, align 8
%stackaddr$prim48084 = alloca %struct.ScmObj*, align 8
%current_45args47255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47254)
store volatile %struct.ScmObj* %current_45args47255, %struct.ScmObj** %stackaddr$prim48084, align 8
%stackaddr$prim48085 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47255)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim48085, align 8
%stackaddr$makeclosure48086 = alloca %struct.ScmObj*, align 8
%fptrToInt48087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42293 to i64
%ae42293 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48087)
store volatile %struct.ScmObj* %ae42293, %struct.ScmObj** %stackaddr$makeclosure48086, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42293, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42293, %struct.ScmObj* %_37foldl140108, i64 1)
%args47535$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48088 = alloca %struct.ScmObj*, align 8
%args47535$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40310, %struct.ScmObj* %args47535$Ycmb40103$0)
store volatile %struct.ScmObj* %args47535$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim48088, align 8
%stackaddr$prim48089 = alloca %struct.ScmObj*, align 8
%args47535$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42293, %struct.ScmObj* %args47535$Ycmb40103$1)
store volatile %struct.ScmObj* %args47535$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim48089, align 8
%clofunc48090 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc48090(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args47535$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae42293(%struct.ScmObj* %env$ae42293,%struct.ScmObj* %current_45args47257) {
%stackaddr$env-ref48091 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42293, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48091
%stackaddr$env-ref48092 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42293, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48092
%stackaddr$prim48093 = alloca %struct.ScmObj*, align 8
%_95k40402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47257)
store volatile %struct.ScmObj* %_95k40402, %struct.ScmObj** %stackaddr$prim48093, align 8
%stackaddr$prim48094 = alloca %struct.ScmObj*, align 8
%current_45args47258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47257)
store volatile %struct.ScmObj* %current_45args47258, %struct.ScmObj** %stackaddr$prim48094, align 8
%stackaddr$prim48095 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47258)
store volatile %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$prim48095, align 8
%stackaddr$makeclosure48096 = alloca %struct.ScmObj*, align 8
%fptrToInt48097 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42295 to i64
%ae42295 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48097)
store volatile %struct.ScmObj* %ae42295, %struct.ScmObj** %stackaddr$makeclosure48096, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42295, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42295, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42295, %struct.ScmObj* %_37foldl40206, i64 2)
%ae42296 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48098 = alloca %struct.ScmObj*, align 8
%fptrToInt48099 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42297 to i64
%ae42297 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48099)
store volatile %struct.ScmObj* %ae42297, %struct.ScmObj** %stackaddr$makeclosure48098, align 8
%args47534$ae42295$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48100 = alloca %struct.ScmObj*, align 8
%args47534$ae42295$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42297, %struct.ScmObj* %args47534$ae42295$0)
store volatile %struct.ScmObj* %args47534$ae42295$1, %struct.ScmObj** %stackaddr$prim48100, align 8
%stackaddr$prim48101 = alloca %struct.ScmObj*, align 8
%args47534$ae42295$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42296, %struct.ScmObj* %args47534$ae42295$1)
store volatile %struct.ScmObj* %args47534$ae42295$2, %struct.ScmObj** %stackaddr$prim48101, align 8
%clofunc48102 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42295)
musttail call tailcc void %clofunc48102(%struct.ScmObj* %ae42295, %struct.ScmObj* %args47534$ae42295$2)
ret void
}

define tailcc void @proc_clo$ae42295(%struct.ScmObj* %env$ae42295,%struct.ScmObj* %current_45args47260) {
%stackaddr$env-ref48103 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42295, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48103
%stackaddr$env-ref48104 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42295, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48104
%stackaddr$env-ref48105 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42295, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48105
%stackaddr$prim48106 = alloca %struct.ScmObj*, align 8
%_95k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47260)
store volatile %struct.ScmObj* %_95k40403, %struct.ScmObj** %stackaddr$prim48106, align 8
%stackaddr$prim48107 = alloca %struct.ScmObj*, align 8
%current_45args47261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47260)
store volatile %struct.ScmObj* %current_45args47261, %struct.ScmObj** %stackaddr$prim48107, align 8
%stackaddr$prim48108 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47261)
store volatile %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$prim48108, align 8
%stackaddr$makeclosure48109 = alloca %struct.ScmObj*, align 8
%fptrToInt48110 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42319 to i64
%ae42319 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48110)
store volatile %struct.ScmObj* %ae42319, %struct.ScmObj** %stackaddr$makeclosure48109, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42319, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42319, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42319, %struct.ScmObj* %_37foldl40206, i64 2)
%ae42320 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48111 = alloca %struct.ScmObj*, align 8
%fptrToInt48112 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42321 to i64
%ae42321 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48112)
store volatile %struct.ScmObj* %ae42321, %struct.ScmObj** %stackaddr$makeclosure48111, align 8
%args47528$ae42319$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48113 = alloca %struct.ScmObj*, align 8
%args47528$ae42319$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42321, %struct.ScmObj* %args47528$ae42319$0)
store volatile %struct.ScmObj* %args47528$ae42319$1, %struct.ScmObj** %stackaddr$prim48113, align 8
%stackaddr$prim48114 = alloca %struct.ScmObj*, align 8
%args47528$ae42319$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42320, %struct.ScmObj* %args47528$ae42319$1)
store volatile %struct.ScmObj* %args47528$ae42319$2, %struct.ScmObj** %stackaddr$prim48114, align 8
%clofunc48115 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42319)
musttail call tailcc void %clofunc48115(%struct.ScmObj* %ae42319, %struct.ScmObj* %args47528$ae42319$2)
ret void
}

define tailcc void @proc_clo$ae42319(%struct.ScmObj* %env$ae42319,%struct.ScmObj* %current_45args47263) {
%stackaddr$env-ref48116 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42319, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48116
%stackaddr$env-ref48117 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42319, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48117
%stackaddr$env-ref48118 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42319, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48118
%stackaddr$prim48119 = alloca %struct.ScmObj*, align 8
%_95k40404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47263)
store volatile %struct.ScmObj* %_95k40404, %struct.ScmObj** %stackaddr$prim48119, align 8
%stackaddr$prim48120 = alloca %struct.ScmObj*, align 8
%current_45args47264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47263)
store volatile %struct.ScmObj* %current_45args47264, %struct.ScmObj** %stackaddr$prim48120, align 8
%stackaddr$prim48121 = alloca %struct.ScmObj*, align 8
%_37_62_6140200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47264)
store volatile %struct.ScmObj* %_37_62_6140200, %struct.ScmObj** %stackaddr$prim48121, align 8
%ae42343 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42344 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48122 = alloca %struct.ScmObj*, align 8
%_37append40196 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42343, %struct.ScmObj* %ae42344)
store volatile %struct.ScmObj* %_37append40196, %struct.ScmObj** %stackaddr$prim48122, align 8
%stackaddr$makeclosure48123 = alloca %struct.ScmObj*, align 8
%fptrToInt48124 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42345 to i64
%ae42345 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48124)
store volatile %struct.ScmObj* %ae42345, %struct.ScmObj** %stackaddr$makeclosure48123, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42345, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42345, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42345, %struct.ScmObj* %_37append40196, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42345, %struct.ScmObj* %_37foldl40206, i64 3)
%ae42346 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48125 = alloca %struct.ScmObj*, align 8
%fptrToInt48126 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42347 to i64
%ae42347 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48126)
store volatile %struct.ScmObj* %ae42347, %struct.ScmObj** %stackaddr$makeclosure48125, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42347, %struct.ScmObj* %_37append40196, i64 0)
%args47522$ae42345$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48127 = alloca %struct.ScmObj*, align 8
%args47522$ae42345$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42347, %struct.ScmObj* %args47522$ae42345$0)
store volatile %struct.ScmObj* %args47522$ae42345$1, %struct.ScmObj** %stackaddr$prim48127, align 8
%stackaddr$prim48128 = alloca %struct.ScmObj*, align 8
%args47522$ae42345$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42346, %struct.ScmObj* %args47522$ae42345$1)
store volatile %struct.ScmObj* %args47522$ae42345$2, %struct.ScmObj** %stackaddr$prim48128, align 8
%clofunc48129 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42345)
musttail call tailcc void %clofunc48129(%struct.ScmObj* %ae42345, %struct.ScmObj* %args47522$ae42345$2)
ret void
}

define tailcc void @proc_clo$ae42345(%struct.ScmObj* %env$ae42345,%struct.ScmObj* %current_45args47266) {
%stackaddr$env-ref48130 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42345, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48130
%stackaddr$env-ref48131 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42345, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48131
%stackaddr$env-ref48132 = alloca %struct.ScmObj*, align 8
%_37append40196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42345, i64 2)
store %struct.ScmObj* %_37append40196, %struct.ScmObj** %stackaddr$env-ref48132
%stackaddr$env-ref48133 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42345, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48133
%stackaddr$prim48134 = alloca %struct.ScmObj*, align 8
%_95k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47266)
store volatile %struct.ScmObj* %_95k40405, %struct.ScmObj** %stackaddr$prim48134, align 8
%stackaddr$prim48135 = alloca %struct.ScmObj*, align 8
%current_45args47267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47266)
store volatile %struct.ScmObj* %current_45args47267, %struct.ScmObj** %stackaddr$prim48135, align 8
%stackaddr$prim48136 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47267)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim48136, align 8
%ae42413 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48137 = alloca %struct.ScmObj*, align 8
%_95040197 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40196, %struct.ScmObj* %ae42413, %struct.ScmObj* %anf_45bind40318)
store volatile %struct.ScmObj* %_95040197, %struct.ScmObj** %stackaddr$prim48137, align 8
%ae42416 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48138 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40196, %struct.ScmObj* %ae42416)
store volatile %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$prim48138, align 8
%stackaddr$makeclosure48139 = alloca %struct.ScmObj*, align 8
%fptrToInt48140 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42417 to i64
%ae42417 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48140)
store volatile %struct.ScmObj* %ae42417, %struct.ScmObj** %stackaddr$makeclosure48139, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42417, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42417, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42417, %struct.ScmObj* %_37foldl40206, i64 2)
%ae42418 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48141 = alloca %struct.ScmObj*, align 8
%fptrToInt48142 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42419 to i64
%ae42419 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48142)
store volatile %struct.ScmObj* %ae42419, %struct.ScmObj** %stackaddr$makeclosure48141, align 8
%args47511$ae42417$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48143 = alloca %struct.ScmObj*, align 8
%args47511$ae42417$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42419, %struct.ScmObj* %args47511$ae42417$0)
store volatile %struct.ScmObj* %args47511$ae42417$1, %struct.ScmObj** %stackaddr$prim48143, align 8
%stackaddr$prim48144 = alloca %struct.ScmObj*, align 8
%args47511$ae42417$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42418, %struct.ScmObj* %args47511$ae42417$1)
store volatile %struct.ScmObj* %args47511$ae42417$2, %struct.ScmObj** %stackaddr$prim48144, align 8
%clofunc48145 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42417)
musttail call tailcc void %clofunc48145(%struct.ScmObj* %ae42417, %struct.ScmObj* %args47511$ae42417$2)
ret void
}

define tailcc void @proc_clo$ae42417(%struct.ScmObj* %env$ae42417,%struct.ScmObj* %current_45args47269) {
%stackaddr$env-ref48146 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42417, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48146
%stackaddr$env-ref48147 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42417, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48147
%stackaddr$env-ref48148 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42417, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48148
%stackaddr$prim48149 = alloca %struct.ScmObj*, align 8
%_95k40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47269)
store volatile %struct.ScmObj* %_95k40406, %struct.ScmObj** %stackaddr$prim48149, align 8
%stackaddr$prim48150 = alloca %struct.ScmObj*, align 8
%current_45args47270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47269)
store volatile %struct.ScmObj* %current_45args47270, %struct.ScmObj** %stackaddr$prim48150, align 8
%stackaddr$prim48151 = alloca %struct.ScmObj*, align 8
%_37list_6340188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47270)
store volatile %struct.ScmObj* %_37list_6340188, %struct.ScmObj** %stackaddr$prim48151, align 8
%stackaddr$makeclosure48152 = alloca %struct.ScmObj*, align 8
%fptrToInt48153 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42833 to i64
%ae42833 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48153)
store volatile %struct.ScmObj* %ae42833, %struct.ScmObj** %stackaddr$makeclosure48152, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42833, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42833, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42833, %struct.ScmObj* %_37foldl40206, i64 2)
%ae42834 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48154 = alloca %struct.ScmObj*, align 8
%fptrToInt48155 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42835 to i64
%ae42835 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48155)
store volatile %struct.ScmObj* %ae42835, %struct.ScmObj** %stackaddr$makeclosure48154, align 8
%args47486$ae42833$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48156 = alloca %struct.ScmObj*, align 8
%args47486$ae42833$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42835, %struct.ScmObj* %args47486$ae42833$0)
store volatile %struct.ScmObj* %args47486$ae42833$1, %struct.ScmObj** %stackaddr$prim48156, align 8
%stackaddr$prim48157 = alloca %struct.ScmObj*, align 8
%args47486$ae42833$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42834, %struct.ScmObj* %args47486$ae42833$1)
store volatile %struct.ScmObj* %args47486$ae42833$2, %struct.ScmObj** %stackaddr$prim48157, align 8
%clofunc48158 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42833)
musttail call tailcc void %clofunc48158(%struct.ScmObj* %ae42833, %struct.ScmObj* %args47486$ae42833$2)
ret void
}

define tailcc void @proc_clo$ae42833(%struct.ScmObj* %env$ae42833,%struct.ScmObj* %current_45args47272) {
%stackaddr$env-ref48159 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42833, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48159
%stackaddr$env-ref48160 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42833, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48160
%stackaddr$env-ref48161 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42833, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48161
%stackaddr$prim48162 = alloca %struct.ScmObj*, align 8
%_95k40407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47272)
store volatile %struct.ScmObj* %_95k40407, %struct.ScmObj** %stackaddr$prim48162, align 8
%stackaddr$prim48163 = alloca %struct.ScmObj*, align 8
%current_45args47273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47272)
store volatile %struct.ScmObj* %current_45args47273, %struct.ScmObj** %stackaddr$prim48163, align 8
%stackaddr$prim48164 = alloca %struct.ScmObj*, align 8
%_37drop40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47273)
store volatile %struct.ScmObj* %_37drop40179, %struct.ScmObj** %stackaddr$prim48164, align 8
%stackaddr$makeclosure48165 = alloca %struct.ScmObj*, align 8
%fptrToInt48166 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43369 to i64
%ae43369 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48166)
store volatile %struct.ScmObj* %ae43369, %struct.ScmObj** %stackaddr$makeclosure48165, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43369, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43369, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43369, %struct.ScmObj* %_37foldl40206, i64 2)
%ae43370 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48167 = alloca %struct.ScmObj*, align 8
%fptrToInt48168 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43371 to i64
%ae43371 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48168)
store volatile %struct.ScmObj* %ae43371, %struct.ScmObj** %stackaddr$makeclosure48167, align 8
%args47462$ae43369$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48169 = alloca %struct.ScmObj*, align 8
%args47462$ae43369$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43371, %struct.ScmObj* %args47462$ae43369$0)
store volatile %struct.ScmObj* %args47462$ae43369$1, %struct.ScmObj** %stackaddr$prim48169, align 8
%stackaddr$prim48170 = alloca %struct.ScmObj*, align 8
%args47462$ae43369$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43370, %struct.ScmObj* %args47462$ae43369$1)
store volatile %struct.ScmObj* %args47462$ae43369$2, %struct.ScmObj** %stackaddr$prim48170, align 8
%clofunc48171 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43369)
musttail call tailcc void %clofunc48171(%struct.ScmObj* %ae43369, %struct.ScmObj* %args47462$ae43369$2)
ret void
}

define tailcc void @proc_clo$ae43369(%struct.ScmObj* %env$ae43369,%struct.ScmObj* %current_45args47275) {
%stackaddr$env-ref48172 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43369, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48172
%stackaddr$env-ref48173 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43369, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48173
%stackaddr$env-ref48174 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43369, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48174
%stackaddr$prim48175 = alloca %struct.ScmObj*, align 8
%_95k40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47275)
store volatile %struct.ScmObj* %_95k40408, %struct.ScmObj** %stackaddr$prim48175, align 8
%stackaddr$prim48176 = alloca %struct.ScmObj*, align 8
%current_45args47276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47275)
store volatile %struct.ScmObj* %current_45args47276, %struct.ScmObj** %stackaddr$prim48176, align 8
%stackaddr$prim48177 = alloca %struct.ScmObj*, align 8
%_37memv40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47276)
store volatile %struct.ScmObj* %_37memv40172, %struct.ScmObj** %stackaddr$prim48177, align 8
%stackaddr$makeclosure48178 = alloca %struct.ScmObj*, align 8
%fptrToInt48179 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43773 to i64
%ae43773 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48179)
store volatile %struct.ScmObj* %ae43773, %struct.ScmObj** %stackaddr$makeclosure48178, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43773, %struct.ScmObj* %_37foldl40206, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43773, %struct.ScmObj* %_37foldr140124, i64 1)
%ae43774 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48180 = alloca %struct.ScmObj*, align 8
%fptrToInt48181 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43775 to i64
%ae43775 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48181)
store volatile %struct.ScmObj* %ae43775, %struct.ScmObj** %stackaddr$makeclosure48180, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43775, %struct.ScmObj* %_37foldl140108, i64 0)
%args47436$ae43773$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48182 = alloca %struct.ScmObj*, align 8
%args47436$ae43773$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43775, %struct.ScmObj* %args47436$ae43773$0)
store volatile %struct.ScmObj* %args47436$ae43773$1, %struct.ScmObj** %stackaddr$prim48182, align 8
%stackaddr$prim48183 = alloca %struct.ScmObj*, align 8
%args47436$ae43773$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43774, %struct.ScmObj* %args47436$ae43773$1)
store volatile %struct.ScmObj* %args47436$ae43773$2, %struct.ScmObj** %stackaddr$prim48183, align 8
%clofunc48184 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43773)
musttail call tailcc void %clofunc48184(%struct.ScmObj* %ae43773, %struct.ScmObj* %args47436$ae43773$2)
ret void
}

define tailcc void @proc_clo$ae43773(%struct.ScmObj* %env$ae43773,%struct.ScmObj* %current_45args47278) {
%stackaddr$env-ref48185 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43773, i64 0)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48185
%stackaddr$env-ref48186 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43773, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48186
%stackaddr$prim48187 = alloca %struct.ScmObj*, align 8
%_95k40409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47278)
store volatile %struct.ScmObj* %_95k40409, %struct.ScmObj** %stackaddr$prim48187, align 8
%stackaddr$prim48188 = alloca %struct.ScmObj*, align 8
%current_45args47279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47278)
store volatile %struct.ScmObj* %current_45args47279, %struct.ScmObj** %stackaddr$prim48188, align 8
%stackaddr$prim48189 = alloca %struct.ScmObj*, align 8
%_37_4740168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47279)
store volatile %struct.ScmObj* %_37_4740168, %struct.ScmObj** %stackaddr$prim48189, align 8
%stackaddr$makeclosure48190 = alloca %struct.ScmObj*, align 8
%fptrToInt48191 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43871 to i64
%ae43871 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48191)
store volatile %struct.ScmObj* %ae43871, %struct.ScmObj** %stackaddr$makeclosure48190, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43871, %struct.ScmObj* %_37foldl40206, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43871, %struct.ScmObj* %_37foldr140124, i64 1)
%ae43872 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48192 = alloca %struct.ScmObj*, align 8
%fptrToInt48193 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43873 to i64
%ae43873 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48193)
store volatile %struct.ScmObj* %ae43873, %struct.ScmObj** %stackaddr$makeclosure48192, align 8
%args47423$ae43871$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48194 = alloca %struct.ScmObj*, align 8
%args47423$ae43871$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43873, %struct.ScmObj* %args47423$ae43871$0)
store volatile %struct.ScmObj* %args47423$ae43871$1, %struct.ScmObj** %stackaddr$prim48194, align 8
%stackaddr$prim48195 = alloca %struct.ScmObj*, align 8
%args47423$ae43871$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43872, %struct.ScmObj* %args47423$ae43871$1)
store volatile %struct.ScmObj* %args47423$ae43871$2, %struct.ScmObj** %stackaddr$prim48195, align 8
%clofunc48196 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43871)
musttail call tailcc void %clofunc48196(%struct.ScmObj* %ae43871, %struct.ScmObj* %args47423$ae43871$2)
ret void
}

define tailcc void @proc_clo$ae43871(%struct.ScmObj* %env$ae43871,%struct.ScmObj* %current_45args47281) {
%stackaddr$env-ref48197 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43871, i64 0)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48197
%stackaddr$env-ref48198 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43871, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48198
%stackaddr$prim48199 = alloca %struct.ScmObj*, align 8
%_95k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47281)
store volatile %struct.ScmObj* %_95k40410, %struct.ScmObj** %stackaddr$prim48199, align 8
%stackaddr$prim48200 = alloca %struct.ScmObj*, align 8
%current_45args47282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47281)
store volatile %struct.ScmObj* %current_45args47282, %struct.ScmObj** %stackaddr$prim48200, align 8
%stackaddr$prim48201 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47282)
store volatile %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$prim48201, align 8
%stackaddr$makeclosure48202 = alloca %struct.ScmObj*, align 8
%fptrToInt48203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43891 to i64
%ae43891 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48203)
store volatile %struct.ScmObj* %ae43891, %struct.ScmObj** %stackaddr$makeclosure48202, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43891, %struct.ScmObj* %_37first40166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43891, %struct.ScmObj* %_37foldl40206, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43891, %struct.ScmObj* %_37foldr140124, i64 2)
%ae43892 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48204 = alloca %struct.ScmObj*, align 8
%fptrToInt48205 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43893 to i64
%ae43893 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48205)
store volatile %struct.ScmObj* %ae43893, %struct.ScmObj** %stackaddr$makeclosure48204, align 8
%args47418$ae43891$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48206 = alloca %struct.ScmObj*, align 8
%args47418$ae43891$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43893, %struct.ScmObj* %args47418$ae43891$0)
store volatile %struct.ScmObj* %args47418$ae43891$1, %struct.ScmObj** %stackaddr$prim48206, align 8
%stackaddr$prim48207 = alloca %struct.ScmObj*, align 8
%args47418$ae43891$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43892, %struct.ScmObj* %args47418$ae43891$1)
store volatile %struct.ScmObj* %args47418$ae43891$2, %struct.ScmObj** %stackaddr$prim48207, align 8
%clofunc48208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43891)
musttail call tailcc void %clofunc48208(%struct.ScmObj* %ae43891, %struct.ScmObj* %args47418$ae43891$2)
ret void
}

define tailcc void @proc_clo$ae43891(%struct.ScmObj* %env$ae43891,%struct.ScmObj* %current_45args47284) {
%stackaddr$env-ref48209 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43891, i64 0)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48209
%stackaddr$env-ref48210 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43891, i64 1)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48210
%stackaddr$env-ref48211 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43891, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48211
%stackaddr$prim48212 = alloca %struct.ScmObj*, align 8
%_95k40411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47284)
store volatile %struct.ScmObj* %_95k40411, %struct.ScmObj** %stackaddr$prim48212, align 8
%stackaddr$prim48213 = alloca %struct.ScmObj*, align 8
%current_45args47285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47284)
store volatile %struct.ScmObj* %current_45args47285, %struct.ScmObj** %stackaddr$prim48213, align 8
%stackaddr$prim48214 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47285)
store volatile %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$prim48214, align 8
%stackaddr$makeclosure48215 = alloca %struct.ScmObj*, align 8
%fptrToInt48216 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43913 to i64
%ae43913 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48216)
store volatile %struct.ScmObj* %ae43913, %struct.ScmObj** %stackaddr$makeclosure48215, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43913, %struct.ScmObj* %_37first40166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43913, %struct.ScmObj* %_37second40164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43913, %struct.ScmObj* %_37foldl40206, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43913, %struct.ScmObj* %_37foldr140124, i64 3)
%ae43914 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48217 = alloca %struct.ScmObj*, align 8
%fptrToInt48218 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43915 to i64
%ae43915 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48218)
store volatile %struct.ScmObj* %ae43915, %struct.ScmObj** %stackaddr$makeclosure48217, align 8
%args47413$ae43913$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48219 = alloca %struct.ScmObj*, align 8
%args47413$ae43913$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43915, %struct.ScmObj* %args47413$ae43913$0)
store volatile %struct.ScmObj* %args47413$ae43913$1, %struct.ScmObj** %stackaddr$prim48219, align 8
%stackaddr$prim48220 = alloca %struct.ScmObj*, align 8
%args47413$ae43913$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43914, %struct.ScmObj* %args47413$ae43913$1)
store volatile %struct.ScmObj* %args47413$ae43913$2, %struct.ScmObj** %stackaddr$prim48220, align 8
%clofunc48221 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43913)
musttail call tailcc void %clofunc48221(%struct.ScmObj* %ae43913, %struct.ScmObj* %args47413$ae43913$2)
ret void
}

define tailcc void @proc_clo$ae43913(%struct.ScmObj* %env$ae43913,%struct.ScmObj* %current_45args47287) {
%stackaddr$env-ref48222 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43913, i64 0)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48222
%stackaddr$env-ref48223 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43913, i64 1)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48223
%stackaddr$env-ref48224 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43913, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48224
%stackaddr$env-ref48225 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43913, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48225
%stackaddr$prim48226 = alloca %struct.ScmObj*, align 8
%_95k40412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47287)
store volatile %struct.ScmObj* %_95k40412, %struct.ScmObj** %stackaddr$prim48226, align 8
%stackaddr$prim48227 = alloca %struct.ScmObj*, align 8
%current_45args47288 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47287)
store volatile %struct.ScmObj* %current_45args47288, %struct.ScmObj** %stackaddr$prim48227, align 8
%stackaddr$prim48228 = alloca %struct.ScmObj*, align 8
%_37third40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47288)
store volatile %struct.ScmObj* %_37third40162, %struct.ScmObj** %stackaddr$prim48228, align 8
%stackaddr$makeclosure48229 = alloca %struct.ScmObj*, align 8
%fptrToInt48230 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43937 to i64
%ae43937 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48230)
store volatile %struct.ScmObj* %ae43937, %struct.ScmObj** %stackaddr$makeclosure48229, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43937, %struct.ScmObj* %_37first40166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43937, %struct.ScmObj* %_37second40164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43937, %struct.ScmObj* %_37foldl40206, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43937, %struct.ScmObj* %_37foldr140124, i64 3)
%ae43938 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48231 = alloca %struct.ScmObj*, align 8
%fptrToInt48232 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43939 to i64
%ae43939 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48232)
store volatile %struct.ScmObj* %ae43939, %struct.ScmObj** %stackaddr$makeclosure48231, align 8
%args47408$ae43937$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48233 = alloca %struct.ScmObj*, align 8
%args47408$ae43937$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43939, %struct.ScmObj* %args47408$ae43937$0)
store volatile %struct.ScmObj* %args47408$ae43937$1, %struct.ScmObj** %stackaddr$prim48233, align 8
%stackaddr$prim48234 = alloca %struct.ScmObj*, align 8
%args47408$ae43937$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43938, %struct.ScmObj* %args47408$ae43937$1)
store volatile %struct.ScmObj* %args47408$ae43937$2, %struct.ScmObj** %stackaddr$prim48234, align 8
%clofunc48235 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43937)
musttail call tailcc void %clofunc48235(%struct.ScmObj* %ae43937, %struct.ScmObj* %args47408$ae43937$2)
ret void
}

define tailcc void @proc_clo$ae43937(%struct.ScmObj* %env$ae43937,%struct.ScmObj* %current_45args47290) {
%stackaddr$env-ref48236 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43937, i64 0)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48236
%stackaddr$env-ref48237 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43937, i64 1)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48237
%stackaddr$env-ref48238 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43937, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48238
%stackaddr$env-ref48239 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43937, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48239
%stackaddr$prim48240 = alloca %struct.ScmObj*, align 8
%_95k40413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47290)
store volatile %struct.ScmObj* %_95k40413, %struct.ScmObj** %stackaddr$prim48240, align 8
%stackaddr$prim48241 = alloca %struct.ScmObj*, align 8
%current_45args47291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47290)
store volatile %struct.ScmObj* %current_45args47291, %struct.ScmObj** %stackaddr$prim48241, align 8
%stackaddr$prim48242 = alloca %struct.ScmObj*, align 8
%_37fourth40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47291)
store volatile %struct.ScmObj* %_37fourth40160, %struct.ScmObj** %stackaddr$prim48242, align 8
%stackaddr$makeclosure48243 = alloca %struct.ScmObj*, align 8
%fptrToInt48244 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43963 to i64
%ae43963 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48244)
store volatile %struct.ScmObj* %ae43963, %struct.ScmObj** %stackaddr$makeclosure48243, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43963, %struct.ScmObj* %_37first40166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43963, %struct.ScmObj* %_37second40164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43963, %struct.ScmObj* %_37foldl40206, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43963, %struct.ScmObj* %_37foldr140124, i64 3)
%ae43964 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48245 = alloca %struct.ScmObj*, align 8
%fptrToInt48246 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43965 to i64
%ae43965 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48246)
store volatile %struct.ScmObj* %ae43965, %struct.ScmObj** %stackaddr$makeclosure48245, align 8
%args47403$ae43963$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48247 = alloca %struct.ScmObj*, align 8
%args47403$ae43963$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43965, %struct.ScmObj* %args47403$ae43963$0)
store volatile %struct.ScmObj* %args47403$ae43963$1, %struct.ScmObj** %stackaddr$prim48247, align 8
%stackaddr$prim48248 = alloca %struct.ScmObj*, align 8
%args47403$ae43963$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43964, %struct.ScmObj* %args47403$ae43963$1)
store volatile %struct.ScmObj* %args47403$ae43963$2, %struct.ScmObj** %stackaddr$prim48248, align 8
%clofunc48249 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43963)
musttail call tailcc void %clofunc48249(%struct.ScmObj* %ae43963, %struct.ScmObj* %args47403$ae43963$2)
ret void
}

define tailcc void @proc_clo$ae43963(%struct.ScmObj* %env$ae43963,%struct.ScmObj* %current_45args47293) {
%stackaddr$env-ref48250 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43963, i64 0)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48250
%stackaddr$env-ref48251 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43963, i64 1)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48251
%stackaddr$env-ref48252 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43963, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48252
%stackaddr$env-ref48253 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43963, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48253
%stackaddr$prim48254 = alloca %struct.ScmObj*, align 8
%_95k40414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47293)
store volatile %struct.ScmObj* %_95k40414, %struct.ScmObj** %stackaddr$prim48254, align 8
%stackaddr$prim48255 = alloca %struct.ScmObj*, align 8
%current_45args47294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47293)
store volatile %struct.ScmObj* %current_45args47294, %struct.ScmObj** %stackaddr$prim48255, align 8
%stackaddr$prim48256 = alloca %struct.ScmObj*, align 8
%promise_6340221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47294)
store volatile %struct.ScmObj* %promise_6340221, %struct.ScmObj** %stackaddr$prim48256, align 8
%stackaddr$makeclosure48257 = alloca %struct.ScmObj*, align 8
%fptrToInt48258 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44050 to i64
%ae44050 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48258)
store volatile %struct.ScmObj* %ae44050, %struct.ScmObj** %stackaddr$makeclosure48257, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44050, %struct.ScmObj* %_37first40166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44050, %struct.ScmObj* %_37second40164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44050, %struct.ScmObj* %_37foldl40206, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44050, %struct.ScmObj* %_37foldr140124, i64 3)
%ae44051 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48259 = alloca %struct.ScmObj*, align 8
%fptrToInt48260 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44052 to i64
%ae44052 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48260)
store volatile %struct.ScmObj* %ae44052, %struct.ScmObj** %stackaddr$makeclosure48259, align 8
%args47396$ae44050$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48261 = alloca %struct.ScmObj*, align 8
%args47396$ae44050$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44052, %struct.ScmObj* %args47396$ae44050$0)
store volatile %struct.ScmObj* %args47396$ae44050$1, %struct.ScmObj** %stackaddr$prim48261, align 8
%stackaddr$prim48262 = alloca %struct.ScmObj*, align 8
%args47396$ae44050$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44051, %struct.ScmObj* %args47396$ae44050$1)
store volatile %struct.ScmObj* %args47396$ae44050$2, %struct.ScmObj** %stackaddr$prim48262, align 8
%clofunc48263 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44050)
musttail call tailcc void %clofunc48263(%struct.ScmObj* %ae44050, %struct.ScmObj* %args47396$ae44050$2)
ret void
}

define tailcc void @proc_clo$ae44050(%struct.ScmObj* %env$ae44050,%struct.ScmObj* %current_45args47296) {
%stackaddr$env-ref48264 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44050, i64 0)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48264
%stackaddr$env-ref48265 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44050, i64 1)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48265
%stackaddr$env-ref48266 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44050, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48266
%stackaddr$env-ref48267 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44050, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48267
%stackaddr$prim48268 = alloca %struct.ScmObj*, align 8
%_95k40415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47296)
store volatile %struct.ScmObj* %_95k40415, %struct.ScmObj** %stackaddr$prim48268, align 8
%stackaddr$prim48269 = alloca %struct.ScmObj*, align 8
%current_45args47297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47296)
store volatile %struct.ScmObj* %current_45args47297, %struct.ScmObj** %stackaddr$prim48269, align 8
%stackaddr$prim48270 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47297)
store volatile %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$prim48270, align 8
%stackaddr$makeclosure48271 = alloca %struct.ScmObj*, align 8
%fptrToInt48272 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44088 to i64
%ae44088 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48272)
store volatile %struct.ScmObj* %ae44088, %struct.ScmObj** %stackaddr$makeclosure48271, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44088, %struct.ScmObj* %_37first40166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44088, %struct.ScmObj* %_37second40164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44088, %struct.ScmObj* %_37foldl40206, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44088, %struct.ScmObj* %_37foldr140124, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44088, %struct.ScmObj* %anf_45bind40361, i64 4)
%ae44089 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48273 = alloca %struct.ScmObj*, align 8
%fptrToInt48274 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44090 to i64
%ae44090 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48274)
store volatile %struct.ScmObj* %ae44090, %struct.ScmObj** %stackaddr$makeclosure48273, align 8
%args47386$ae44088$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48275 = alloca %struct.ScmObj*, align 8
%args47386$ae44088$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44090, %struct.ScmObj* %args47386$ae44088$0)
store volatile %struct.ScmObj* %args47386$ae44088$1, %struct.ScmObj** %stackaddr$prim48275, align 8
%stackaddr$prim48276 = alloca %struct.ScmObj*, align 8
%args47386$ae44088$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44089, %struct.ScmObj* %args47386$ae44088$1)
store volatile %struct.ScmObj* %args47386$ae44088$2, %struct.ScmObj** %stackaddr$prim48276, align 8
%clofunc48277 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44088)
musttail call tailcc void %clofunc48277(%struct.ScmObj* %ae44088, %struct.ScmObj* %args47386$ae44088$2)
ret void
}

define tailcc void @proc_clo$ae44088(%struct.ScmObj* %env$ae44088,%struct.ScmObj* %current_45args47299) {
%stackaddr$env-ref48278 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44088, i64 0)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48278
%stackaddr$env-ref48279 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44088, i64 1)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48279
%stackaddr$env-ref48280 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44088, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48280
%stackaddr$env-ref48281 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44088, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48281
%stackaddr$env-ref48282 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44088, i64 4)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48282
%stackaddr$prim48283 = alloca %struct.ScmObj*, align 8
%_95k40416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47299)
store volatile %struct.ScmObj* %_95k40416, %struct.ScmObj** %stackaddr$prim48283, align 8
%stackaddr$prim48284 = alloca %struct.ScmObj*, align 8
%current_45args47300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47299)
store volatile %struct.ScmObj* %current_45args47300, %struct.ScmObj** %stackaddr$prim48284, align 8
%stackaddr$prim48285 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47300)
store volatile %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$prim48285, align 8
%stackaddr$makeclosure48286 = alloca %struct.ScmObj*, align 8
%fptrToInt48287 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44110 to i64
%ae44110 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48287)
store volatile %struct.ScmObj* %ae44110, %struct.ScmObj** %stackaddr$makeclosure48286, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44110, %struct.ScmObj* %_37first40166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44110, %struct.ScmObj* %_37second40164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44110, %struct.ScmObj* %_37foldl40206, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44110, %struct.ScmObj* %_37foldr140124, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44110, %struct.ScmObj* %anf_45bind40362, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44110, %struct.ScmObj* %anf_45bind40361, i64 5)
%ae44111 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48288 = alloca %struct.ScmObj*, align 8
%fptrToInt48289 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44112 to i64
%ae44112 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48289)
store volatile %struct.ScmObj* %ae44112, %struct.ScmObj** %stackaddr$makeclosure48288, align 8
%args47384$ae44110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48290 = alloca %struct.ScmObj*, align 8
%args47384$ae44110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44112, %struct.ScmObj* %args47384$ae44110$0)
store volatile %struct.ScmObj* %args47384$ae44110$1, %struct.ScmObj** %stackaddr$prim48290, align 8
%stackaddr$prim48291 = alloca %struct.ScmObj*, align 8
%args47384$ae44110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44111, %struct.ScmObj* %args47384$ae44110$1)
store volatile %struct.ScmObj* %args47384$ae44110$2, %struct.ScmObj** %stackaddr$prim48291, align 8
%clofunc48292 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44110)
musttail call tailcc void %clofunc48292(%struct.ScmObj* %ae44110, %struct.ScmObj* %args47384$ae44110$2)
ret void
}

define tailcc void @proc_clo$ae44110(%struct.ScmObj* %env$ae44110,%struct.ScmObj* %current_45args47302) {
%stackaddr$env-ref48293 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44110, i64 0)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48293
%stackaddr$env-ref48294 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44110, i64 1)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48294
%stackaddr$env-ref48295 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44110, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48295
%stackaddr$env-ref48296 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44110, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48296
%stackaddr$env-ref48297 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44110, i64 4)
store %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$env-ref48297
%stackaddr$env-ref48298 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44110, i64 5)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48298
%stackaddr$prim48299 = alloca %struct.ScmObj*, align 8
%_95k40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47302)
store volatile %struct.ScmObj* %_95k40417, %struct.ScmObj** %stackaddr$prim48299, align 8
%stackaddr$prim48300 = alloca %struct.ScmObj*, align 8
%current_45args47303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47302)
store volatile %struct.ScmObj* %current_45args47303, %struct.ScmObj** %stackaddr$prim48300, align 8
%stackaddr$prim48301 = alloca %struct.ScmObj*, align 8
%anf_45bind40363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47303)
store volatile %struct.ScmObj* %anf_45bind40363, %struct.ScmObj** %stackaddr$prim48301, align 8
%stackaddr$makeclosure48302 = alloca %struct.ScmObj*, align 8
%fptrToInt48303 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44133 to i64
%ae44133 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48303)
store volatile %struct.ScmObj* %ae44133, %struct.ScmObj** %stackaddr$makeclosure48302, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %_37first40166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %_37second40164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %_37foldl40206, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %_37foldr140124, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %anf_45bind40362, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %anf_45bind40361, i64 5)
%ae44134 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44135 = call %struct.ScmObj* @const_init_int(i64 1)
%ae44136 = call %struct.ScmObj* @const_init_int(i64 2)
%ae44137 = call %struct.ScmObj* @const_init_int(i64 3)
%args47382$anf_45bind40363$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48304 = alloca %struct.ScmObj*, align 8
%args47382$anf_45bind40363$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44137, %struct.ScmObj* %args47382$anf_45bind40363$0)
store volatile %struct.ScmObj* %args47382$anf_45bind40363$1, %struct.ScmObj** %stackaddr$prim48304, align 8
%stackaddr$prim48305 = alloca %struct.ScmObj*, align 8
%args47382$anf_45bind40363$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44136, %struct.ScmObj* %args47382$anf_45bind40363$1)
store volatile %struct.ScmObj* %args47382$anf_45bind40363$2, %struct.ScmObj** %stackaddr$prim48305, align 8
%stackaddr$prim48306 = alloca %struct.ScmObj*, align 8
%args47382$anf_45bind40363$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44135, %struct.ScmObj* %args47382$anf_45bind40363$2)
store volatile %struct.ScmObj* %args47382$anf_45bind40363$3, %struct.ScmObj** %stackaddr$prim48306, align 8
%stackaddr$prim48307 = alloca %struct.ScmObj*, align 8
%args47382$anf_45bind40363$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44134, %struct.ScmObj* %args47382$anf_45bind40363$3)
store volatile %struct.ScmObj* %args47382$anf_45bind40363$4, %struct.ScmObj** %stackaddr$prim48307, align 8
%stackaddr$prim48308 = alloca %struct.ScmObj*, align 8
%args47382$anf_45bind40363$5 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44133, %struct.ScmObj* %args47382$anf_45bind40363$4)
store volatile %struct.ScmObj* %args47382$anf_45bind40363$5, %struct.ScmObj** %stackaddr$prim48308, align 8
%clofunc48309 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40363)
musttail call tailcc void %clofunc48309(%struct.ScmObj* %anf_45bind40363, %struct.ScmObj* %args47382$anf_45bind40363$5)
ret void
}

define tailcc void @proc_clo$ae44133(%struct.ScmObj* %env$ae44133,%struct.ScmObj* %current_45args47305) {
%stackaddr$env-ref48310 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 0)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48310
%stackaddr$env-ref48311 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 1)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48311
%stackaddr$env-ref48312 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48312
%stackaddr$env-ref48313 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48313
%stackaddr$env-ref48314 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 4)
store %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$env-ref48314
%stackaddr$env-ref48315 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 5)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48315
%stackaddr$prim48316 = alloca %struct.ScmObj*, align 8
%_95k40418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47305)
store volatile %struct.ScmObj* %_95k40418, %struct.ScmObj** %stackaddr$prim48316, align 8
%stackaddr$prim48317 = alloca %struct.ScmObj*, align 8
%current_45args47306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47305)
store volatile %struct.ScmObj* %current_45args47306, %struct.ScmObj** %stackaddr$prim48317, align 8
%stackaddr$prim48318 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47306)
store volatile %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$prim48318, align 8
%stackaddr$makeclosure48319 = alloca %struct.ScmObj*, align 8
%fptrToInt48320 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44162 to i64
%ae44162 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48320)
store volatile %struct.ScmObj* %ae44162, %struct.ScmObj** %stackaddr$makeclosure48319, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %anf_45bind40364, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %_37first40166, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %_37second40164, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %_37foldl40206, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %anf_45bind40362, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %anf_45bind40361, i64 6)
%ae44163 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48321 = alloca %struct.ScmObj*, align 8
%fptrToInt48322 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44164 to i64
%ae44164 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48322)
store volatile %struct.ScmObj* %ae44164, %struct.ScmObj** %stackaddr$makeclosure48321, align 8
%args47381$ae44162$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48323 = alloca %struct.ScmObj*, align 8
%args47381$ae44162$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44164, %struct.ScmObj* %args47381$ae44162$0)
store volatile %struct.ScmObj* %args47381$ae44162$1, %struct.ScmObj** %stackaddr$prim48323, align 8
%stackaddr$prim48324 = alloca %struct.ScmObj*, align 8
%args47381$ae44162$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44163, %struct.ScmObj* %args47381$ae44162$1)
store volatile %struct.ScmObj* %args47381$ae44162$2, %struct.ScmObj** %stackaddr$prim48324, align 8
%clofunc48325 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44162)
musttail call tailcc void %clofunc48325(%struct.ScmObj* %ae44162, %struct.ScmObj* %args47381$ae44162$2)
ret void
}

define tailcc void @proc_clo$ae44162(%struct.ScmObj* %env$ae44162,%struct.ScmObj* %current_45args47308) {
%stackaddr$env-ref48326 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48326
%stackaddr$env-ref48327 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 1)
store %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$env-ref48327
%stackaddr$env-ref48328 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 2)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48328
%stackaddr$env-ref48329 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 3)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48329
%stackaddr$env-ref48330 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 4)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48330
%stackaddr$env-ref48331 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 5)
store %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$env-ref48331
%stackaddr$env-ref48332 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 6)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48332
%stackaddr$prim48333 = alloca %struct.ScmObj*, align 8
%_95k40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47308)
store volatile %struct.ScmObj* %_95k40419, %struct.ScmObj** %stackaddr$prim48333, align 8
%stackaddr$prim48334 = alloca %struct.ScmObj*, align 8
%current_45args47309 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47308)
store volatile %struct.ScmObj* %current_45args47309, %struct.ScmObj** %stackaddr$prim48334, align 8
%stackaddr$prim48335 = alloca %struct.ScmObj*, align 8
%anf_45bind40365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47309)
store volatile %struct.ScmObj* %anf_45bind40365, %struct.ScmObj** %stackaddr$prim48335, align 8
%stackaddr$makeclosure48336 = alloca %struct.ScmObj*, align 8
%fptrToInt48337 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44185 to i64
%ae44185 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48337)
store volatile %struct.ScmObj* %ae44185, %struct.ScmObj** %stackaddr$makeclosure48336, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %anf_45bind40364, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %_37first40166, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %_37second40164, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %_37foldl40206, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %anf_45bind40362, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %anf_45bind40361, i64 6)
%ae44186 = call %struct.ScmObj* @const_init_int(i64 4)
%ae44187 = call %struct.ScmObj* @const_init_int(i64 5)
%ae44188 = call %struct.ScmObj* @const_init_int(i64 6)
%ae44189 = call %struct.ScmObj* @const_init_int(i64 7)
%args47379$anf_45bind40365$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48338 = alloca %struct.ScmObj*, align 8
%args47379$anf_45bind40365$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44189, %struct.ScmObj* %args47379$anf_45bind40365$0)
store volatile %struct.ScmObj* %args47379$anf_45bind40365$1, %struct.ScmObj** %stackaddr$prim48338, align 8
%stackaddr$prim48339 = alloca %struct.ScmObj*, align 8
%args47379$anf_45bind40365$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44188, %struct.ScmObj* %args47379$anf_45bind40365$1)
store volatile %struct.ScmObj* %args47379$anf_45bind40365$2, %struct.ScmObj** %stackaddr$prim48339, align 8
%stackaddr$prim48340 = alloca %struct.ScmObj*, align 8
%args47379$anf_45bind40365$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44187, %struct.ScmObj* %args47379$anf_45bind40365$2)
store volatile %struct.ScmObj* %args47379$anf_45bind40365$3, %struct.ScmObj** %stackaddr$prim48340, align 8
%stackaddr$prim48341 = alloca %struct.ScmObj*, align 8
%args47379$anf_45bind40365$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44186, %struct.ScmObj* %args47379$anf_45bind40365$3)
store volatile %struct.ScmObj* %args47379$anf_45bind40365$4, %struct.ScmObj** %stackaddr$prim48341, align 8
%stackaddr$prim48342 = alloca %struct.ScmObj*, align 8
%args47379$anf_45bind40365$5 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44185, %struct.ScmObj* %args47379$anf_45bind40365$4)
store volatile %struct.ScmObj* %args47379$anf_45bind40365$5, %struct.ScmObj** %stackaddr$prim48342, align 8
%clofunc48343 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40365)
musttail call tailcc void %clofunc48343(%struct.ScmObj* %anf_45bind40365, %struct.ScmObj* %args47379$anf_45bind40365$5)
ret void
}

define tailcc void @proc_clo$ae44185(%struct.ScmObj* %env$ae44185,%struct.ScmObj* %current_45args47311) {
%stackaddr$env-ref48344 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48344
%stackaddr$env-ref48345 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 1)
store %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$env-ref48345
%stackaddr$env-ref48346 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 2)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48346
%stackaddr$env-ref48347 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 3)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48347
%stackaddr$env-ref48348 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 4)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48348
%stackaddr$env-ref48349 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 5)
store %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$env-ref48349
%stackaddr$env-ref48350 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 6)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48350
%stackaddr$prim48351 = alloca %struct.ScmObj*, align 8
%_95k40420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47311)
store volatile %struct.ScmObj* %_95k40420, %struct.ScmObj** %stackaddr$prim48351, align 8
%stackaddr$prim48352 = alloca %struct.ScmObj*, align 8
%current_45args47312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47311)
store volatile %struct.ScmObj* %current_45args47312, %struct.ScmObj** %stackaddr$prim48352, align 8
%stackaddr$prim48353 = alloca %struct.ScmObj*, align 8
%anf_45bind40366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47312)
store volatile %struct.ScmObj* %anf_45bind40366, %struct.ScmObj** %stackaddr$prim48353, align 8
%stackaddr$makeclosure48354 = alloca %struct.ScmObj*, align 8
%fptrToInt48355 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44214 to i64
%ae44214 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48355)
store volatile %struct.ScmObj* %ae44214, %struct.ScmObj** %stackaddr$makeclosure48354, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44214, %struct.ScmObj* %_37foldl40206, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44214, %struct.ScmObj* %anf_45bind40366, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44214, %struct.ScmObj* %_37foldr140124, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44214, %struct.ScmObj* %anf_45bind40364, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44214, %struct.ScmObj* %_37first40166, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44214, %struct.ScmObj* %_37second40164, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44214, %struct.ScmObj* %anf_45bind40362, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44214, %struct.ScmObj* %anf_45bind40361, i64 7)
%ae44215 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48356 = alloca %struct.ScmObj*, align 8
%fptrToInt48357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44216 to i64
%ae44216 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48357)
store volatile %struct.ScmObj* %ae44216, %struct.ScmObj** %stackaddr$makeclosure48356, align 8
%args47378$ae44214$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48358 = alloca %struct.ScmObj*, align 8
%args47378$ae44214$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44216, %struct.ScmObj* %args47378$ae44214$0)
store volatile %struct.ScmObj* %args47378$ae44214$1, %struct.ScmObj** %stackaddr$prim48358, align 8
%stackaddr$prim48359 = alloca %struct.ScmObj*, align 8
%args47378$ae44214$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44215, %struct.ScmObj* %args47378$ae44214$1)
store volatile %struct.ScmObj* %args47378$ae44214$2, %struct.ScmObj** %stackaddr$prim48359, align 8
%clofunc48360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44214)
musttail call tailcc void %clofunc48360(%struct.ScmObj* %ae44214, %struct.ScmObj* %args47378$ae44214$2)
ret void
}

define tailcc void @proc_clo$ae44214(%struct.ScmObj* %env$ae44214,%struct.ScmObj* %current_45args47314) {
%stackaddr$env-ref48361 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44214, i64 0)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48361
%stackaddr$env-ref48362 = alloca %struct.ScmObj*, align 8
%anf_45bind40366 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44214, i64 1)
store %struct.ScmObj* %anf_45bind40366, %struct.ScmObj** %stackaddr$env-ref48362
%stackaddr$env-ref48363 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44214, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48363
%stackaddr$env-ref48364 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44214, i64 3)
store %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$env-ref48364
%stackaddr$env-ref48365 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44214, i64 4)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48365
%stackaddr$env-ref48366 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44214, i64 5)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48366
%stackaddr$env-ref48367 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44214, i64 6)
store %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$env-ref48367
%stackaddr$env-ref48368 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44214, i64 7)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48368
%stackaddr$prim48369 = alloca %struct.ScmObj*, align 8
%_95k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47314)
store volatile %struct.ScmObj* %_95k40421, %struct.ScmObj** %stackaddr$prim48369, align 8
%stackaddr$prim48370 = alloca %struct.ScmObj*, align 8
%current_45args47315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47314)
store volatile %struct.ScmObj* %current_45args47315, %struct.ScmObj** %stackaddr$prim48370, align 8
%stackaddr$prim48371 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47315)
store volatile %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$prim48371, align 8
%stackaddr$makeclosure48372 = alloca %struct.ScmObj*, align 8
%fptrToInt48373 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44237 to i64
%ae44237 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48373)
store volatile %struct.ScmObj* %ae44237, %struct.ScmObj** %stackaddr$makeclosure48372, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %_37foldl40206, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %anf_45bind40366, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %_37foldr140124, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %anf_45bind40364, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %_37first40166, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %_37second40164, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %anf_45bind40362, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %anf_45bind40361, i64 7)
%ae44238 = call %struct.ScmObj* @const_init_int(i64 2)
%ae44239 = call %struct.ScmObj* @const_init_int(i64 5)
%ae44240 = call %struct.ScmObj* @const_init_int(i64 7)
%ae44241 = call %struct.ScmObj* @const_init_int(i64 9)
%args47376$anf_45bind40367$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48374 = alloca %struct.ScmObj*, align 8
%args47376$anf_45bind40367$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44241, %struct.ScmObj* %args47376$anf_45bind40367$0)
store volatile %struct.ScmObj* %args47376$anf_45bind40367$1, %struct.ScmObj** %stackaddr$prim48374, align 8
%stackaddr$prim48375 = alloca %struct.ScmObj*, align 8
%args47376$anf_45bind40367$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44240, %struct.ScmObj* %args47376$anf_45bind40367$1)
store volatile %struct.ScmObj* %args47376$anf_45bind40367$2, %struct.ScmObj** %stackaddr$prim48375, align 8
%stackaddr$prim48376 = alloca %struct.ScmObj*, align 8
%args47376$anf_45bind40367$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44239, %struct.ScmObj* %args47376$anf_45bind40367$2)
store volatile %struct.ScmObj* %args47376$anf_45bind40367$3, %struct.ScmObj** %stackaddr$prim48376, align 8
%stackaddr$prim48377 = alloca %struct.ScmObj*, align 8
%args47376$anf_45bind40367$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44238, %struct.ScmObj* %args47376$anf_45bind40367$3)
store volatile %struct.ScmObj* %args47376$anf_45bind40367$4, %struct.ScmObj** %stackaddr$prim48377, align 8
%stackaddr$prim48378 = alloca %struct.ScmObj*, align 8
%args47376$anf_45bind40367$5 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44237, %struct.ScmObj* %args47376$anf_45bind40367$4)
store volatile %struct.ScmObj* %args47376$anf_45bind40367$5, %struct.ScmObj** %stackaddr$prim48378, align 8
%clofunc48379 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40367)
musttail call tailcc void %clofunc48379(%struct.ScmObj* %anf_45bind40367, %struct.ScmObj* %args47376$anf_45bind40367$5)
ret void
}

define tailcc void @proc_clo$ae44237(%struct.ScmObj* %env$ae44237,%struct.ScmObj* %current_45args47317) {
%stackaddr$env-ref48380 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 0)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48380
%stackaddr$env-ref48381 = alloca %struct.ScmObj*, align 8
%anf_45bind40366 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 1)
store %struct.ScmObj* %anf_45bind40366, %struct.ScmObj** %stackaddr$env-ref48381
%stackaddr$env-ref48382 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48382
%stackaddr$env-ref48383 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 3)
store %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$env-ref48383
%stackaddr$env-ref48384 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 4)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48384
%stackaddr$env-ref48385 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 5)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48385
%stackaddr$env-ref48386 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 6)
store %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$env-ref48386
%stackaddr$env-ref48387 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 7)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48387
%stackaddr$prim48388 = alloca %struct.ScmObj*, align 8
%_95k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47317)
store volatile %struct.ScmObj* %_95k40422, %struct.ScmObj** %stackaddr$prim48388, align 8
%stackaddr$prim48389 = alloca %struct.ScmObj*, align 8
%current_45args47318 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47317)
store volatile %struct.ScmObj* %current_45args47318, %struct.ScmObj** %stackaddr$prim48389, align 8
%stackaddr$prim48390 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47318)
store volatile %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$prim48390, align 8
%stackaddr$makeclosure48391 = alloca %struct.ScmObj*, align 8
%fptrToInt48392 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44267 to i64
%ae44267 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48392)
store volatile %struct.ScmObj* %ae44267, %struct.ScmObj** %stackaddr$makeclosure48391, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44267, %struct.ScmObj* %_37first40166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44267, %struct.ScmObj* %_37second40164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44267, %struct.ScmObj* %_37foldl40206, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44267, %struct.ScmObj* %_37foldr140124, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44267, %struct.ScmObj* %anf_45bind40361, i64 4)
%args47375$anf_45bind40362$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48393 = alloca %struct.ScmObj*, align 8
%args47375$anf_45bind40362$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40368, %struct.ScmObj* %args47375$anf_45bind40362$0)
store volatile %struct.ScmObj* %args47375$anf_45bind40362$1, %struct.ScmObj** %stackaddr$prim48393, align 8
%stackaddr$prim48394 = alloca %struct.ScmObj*, align 8
%args47375$anf_45bind40362$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40366, %struct.ScmObj* %args47375$anf_45bind40362$1)
store volatile %struct.ScmObj* %args47375$anf_45bind40362$2, %struct.ScmObj** %stackaddr$prim48394, align 8
%stackaddr$prim48395 = alloca %struct.ScmObj*, align 8
%args47375$anf_45bind40362$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40364, %struct.ScmObj* %args47375$anf_45bind40362$2)
store volatile %struct.ScmObj* %args47375$anf_45bind40362$3, %struct.ScmObj** %stackaddr$prim48395, align 8
%stackaddr$prim48396 = alloca %struct.ScmObj*, align 8
%args47375$anf_45bind40362$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44267, %struct.ScmObj* %args47375$anf_45bind40362$3)
store volatile %struct.ScmObj* %args47375$anf_45bind40362$4, %struct.ScmObj** %stackaddr$prim48396, align 8
%clofunc48397 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40362)
musttail call tailcc void %clofunc48397(%struct.ScmObj* %anf_45bind40362, %struct.ScmObj* %args47375$anf_45bind40362$4)
ret void
}

define tailcc void @proc_clo$ae44267(%struct.ScmObj* %env$ae44267,%struct.ScmObj* %current_45args47320) {
%stackaddr$env-ref48398 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44267, i64 0)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48398
%stackaddr$env-ref48399 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44267, i64 1)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48399
%stackaddr$env-ref48400 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44267, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48400
%stackaddr$env-ref48401 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44267, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48401
%stackaddr$env-ref48402 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44267, i64 4)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48402
%stackaddr$prim48403 = alloca %struct.ScmObj*, align 8
%_95k40423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47320)
store volatile %struct.ScmObj* %_95k40423, %struct.ScmObj** %stackaddr$prim48403, align 8
%stackaddr$prim48404 = alloca %struct.ScmObj*, align 8
%current_45args47321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47320)
store volatile %struct.ScmObj* %current_45args47321, %struct.ScmObj** %stackaddr$prim48404, align 8
%stackaddr$prim48405 = alloca %struct.ScmObj*, align 8
%anf_45bind40369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47321)
store volatile %struct.ScmObj* %anf_45bind40369, %struct.ScmObj** %stackaddr$prim48405, align 8
%stackaddr$makeclosure48406 = alloca %struct.ScmObj*, align 8
%fptrToInt48407 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44271 to i64
%ae44271 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48407)
store volatile %struct.ScmObj* %ae44271, %struct.ScmObj** %stackaddr$makeclosure48406, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44271, %struct.ScmObj* %_37first40166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44271, %struct.ScmObj* %_37second40164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44271, %struct.ScmObj* %anf_45bind40369, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44271, %struct.ScmObj* %_37foldl40206, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44271, %struct.ScmObj* %_37foldr140124, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44271, %struct.ScmObj* %anf_45bind40361, i64 5)
%ae44272 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48408 = alloca %struct.ScmObj*, align 8
%fptrToInt48409 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44273 to i64
%ae44273 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48409)
store volatile %struct.ScmObj* %ae44273, %struct.ScmObj** %stackaddr$makeclosure48408, align 8
%args47374$ae44271$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48410 = alloca %struct.ScmObj*, align 8
%args47374$ae44271$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44273, %struct.ScmObj* %args47374$ae44271$0)
store volatile %struct.ScmObj* %args47374$ae44271$1, %struct.ScmObj** %stackaddr$prim48410, align 8
%stackaddr$prim48411 = alloca %struct.ScmObj*, align 8
%args47374$ae44271$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44272, %struct.ScmObj* %args47374$ae44271$1)
store volatile %struct.ScmObj* %args47374$ae44271$2, %struct.ScmObj** %stackaddr$prim48411, align 8
%clofunc48412 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44271)
musttail call tailcc void %clofunc48412(%struct.ScmObj* %ae44271, %struct.ScmObj* %args47374$ae44271$2)
ret void
}

define tailcc void @proc_clo$ae44271(%struct.ScmObj* %env$ae44271,%struct.ScmObj* %current_45args47323) {
%stackaddr$env-ref48413 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44271, i64 0)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48413
%stackaddr$env-ref48414 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44271, i64 1)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48414
%stackaddr$env-ref48415 = alloca %struct.ScmObj*, align 8
%anf_45bind40369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44271, i64 2)
store %struct.ScmObj* %anf_45bind40369, %struct.ScmObj** %stackaddr$env-ref48415
%stackaddr$env-ref48416 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44271, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48416
%stackaddr$env-ref48417 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44271, i64 4)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48417
%stackaddr$env-ref48418 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44271, i64 5)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48418
%stackaddr$prim48419 = alloca %struct.ScmObj*, align 8
%_95k40424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47323)
store volatile %struct.ScmObj* %_95k40424, %struct.ScmObj** %stackaddr$prim48419, align 8
%stackaddr$prim48420 = alloca %struct.ScmObj*, align 8
%current_45args47324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47323)
store volatile %struct.ScmObj* %current_45args47324, %struct.ScmObj** %stackaddr$prim48420, align 8
%stackaddr$prim48421 = alloca %struct.ScmObj*, align 8
%anf_45bind40370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47324)
store volatile %struct.ScmObj* %anf_45bind40370, %struct.ScmObj** %stackaddr$prim48421, align 8
%stackaddr$makeclosure48422 = alloca %struct.ScmObj*, align 8
%fptrToInt48423 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44293 to i64
%ae44293 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48423)
store volatile %struct.ScmObj* %ae44293, %struct.ScmObj** %stackaddr$makeclosure48422, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44293, %struct.ScmObj* %anf_45bind40370, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44293, %struct.ScmObj* %anf_45bind40369, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44293, %struct.ScmObj* %_37foldl40206, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44293, %struct.ScmObj* %_37foldr140124, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44293, %struct.ScmObj* %anf_45bind40361, i64 4)
%ae44294 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48424 = alloca %struct.ScmObj*, align 8
%fptrToInt48425 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44295 to i64
%ae44295 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48425)
store volatile %struct.ScmObj* %ae44295, %struct.ScmObj** %stackaddr$makeclosure48424, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44295, %struct.ScmObj* %_37first40166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44295, %struct.ScmObj* %_37second40164, i64 1)
%args47372$ae44293$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48426 = alloca %struct.ScmObj*, align 8
%args47372$ae44293$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44295, %struct.ScmObj* %args47372$ae44293$0)
store volatile %struct.ScmObj* %args47372$ae44293$1, %struct.ScmObj** %stackaddr$prim48426, align 8
%stackaddr$prim48427 = alloca %struct.ScmObj*, align 8
%args47372$ae44293$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44294, %struct.ScmObj* %args47372$ae44293$1)
store volatile %struct.ScmObj* %args47372$ae44293$2, %struct.ScmObj** %stackaddr$prim48427, align 8
%clofunc48428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44293)
musttail call tailcc void %clofunc48428(%struct.ScmObj* %ae44293, %struct.ScmObj* %args47372$ae44293$2)
ret void
}

define tailcc void @proc_clo$ae44293(%struct.ScmObj* %env$ae44293,%struct.ScmObj* %current_45args47326) {
%stackaddr$env-ref48429 = alloca %struct.ScmObj*, align 8
%anf_45bind40370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44293, i64 0)
store %struct.ScmObj* %anf_45bind40370, %struct.ScmObj** %stackaddr$env-ref48429
%stackaddr$env-ref48430 = alloca %struct.ScmObj*, align 8
%anf_45bind40369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44293, i64 1)
store %struct.ScmObj* %anf_45bind40369, %struct.ScmObj** %stackaddr$env-ref48430
%stackaddr$env-ref48431 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44293, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48431
%stackaddr$env-ref48432 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44293, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48432
%stackaddr$env-ref48433 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44293, i64 4)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48433
%stackaddr$prim48434 = alloca %struct.ScmObj*, align 8
%_95k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47326)
store volatile %struct.ScmObj* %_95k40425, %struct.ScmObj** %stackaddr$prim48434, align 8
%stackaddr$prim48435 = alloca %struct.ScmObj*, align 8
%current_45args47327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47326)
store volatile %struct.ScmObj* %current_45args47327, %struct.ScmObj** %stackaddr$prim48435, align 8
%stackaddr$prim48436 = alloca %struct.ScmObj*, align 8
%anf_45bind40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47327)
store volatile %struct.ScmObj* %anf_45bind40375, %struct.ScmObj** %stackaddr$prim48436, align 8
%stackaddr$makeclosure48437 = alloca %struct.ScmObj*, align 8
%fptrToInt48438 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44365 to i64
%ae44365 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48438)
store volatile %struct.ScmObj* %ae44365, %struct.ScmObj** %stackaddr$makeclosure48437, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44365, %struct.ScmObj* %anf_45bind40375, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44365, %struct.ScmObj* %anf_45bind40370, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44365, %struct.ScmObj* %anf_45bind40369, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44365, %struct.ScmObj* %_37foldl40206, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44365, %struct.ScmObj* %_37foldr140124, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44365, %struct.ScmObj* %anf_45bind40361, i64 5)
%ae44366 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48439 = alloca %struct.ScmObj*, align 8
%fptrToInt48440 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44367 to i64
%ae44367 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48440)
store volatile %struct.ScmObj* %ae44367, %struct.ScmObj** %stackaddr$makeclosure48439, align 8
%args47358$ae44365$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48441 = alloca %struct.ScmObj*, align 8
%args47358$ae44365$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44367, %struct.ScmObj* %args47358$ae44365$0)
store volatile %struct.ScmObj* %args47358$ae44365$1, %struct.ScmObj** %stackaddr$prim48441, align 8
%stackaddr$prim48442 = alloca %struct.ScmObj*, align 8
%args47358$ae44365$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44366, %struct.ScmObj* %args47358$ae44365$1)
store volatile %struct.ScmObj* %args47358$ae44365$2, %struct.ScmObj** %stackaddr$prim48442, align 8
%clofunc48443 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44365)
musttail call tailcc void %clofunc48443(%struct.ScmObj* %ae44365, %struct.ScmObj* %args47358$ae44365$2)
ret void
}

define tailcc void @proc_clo$ae44365(%struct.ScmObj* %env$ae44365,%struct.ScmObj* %current_45args47329) {
%stackaddr$env-ref48444 = alloca %struct.ScmObj*, align 8
%anf_45bind40375 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44365, i64 0)
store %struct.ScmObj* %anf_45bind40375, %struct.ScmObj** %stackaddr$env-ref48444
%stackaddr$env-ref48445 = alloca %struct.ScmObj*, align 8
%anf_45bind40370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44365, i64 1)
store %struct.ScmObj* %anf_45bind40370, %struct.ScmObj** %stackaddr$env-ref48445
%stackaddr$env-ref48446 = alloca %struct.ScmObj*, align 8
%anf_45bind40369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44365, i64 2)
store %struct.ScmObj* %anf_45bind40369, %struct.ScmObj** %stackaddr$env-ref48446
%stackaddr$env-ref48447 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44365, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48447
%stackaddr$env-ref48448 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44365, i64 4)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48448
%stackaddr$env-ref48449 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44365, i64 5)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48449
%stackaddr$prim48450 = alloca %struct.ScmObj*, align 8
%_95k40426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47329)
store volatile %struct.ScmObj* %_95k40426, %struct.ScmObj** %stackaddr$prim48450, align 8
%stackaddr$prim48451 = alloca %struct.ScmObj*, align 8
%current_45args47330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47329)
store volatile %struct.ScmObj* %current_45args47330, %struct.ScmObj** %stackaddr$prim48451, align 8
%stackaddr$prim48452 = alloca %struct.ScmObj*, align 8
%anf_45bind40378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47330)
store volatile %struct.ScmObj* %anf_45bind40378, %struct.ScmObj** %stackaddr$prim48452, align 8
%stackaddr$makeclosure48453 = alloca %struct.ScmObj*, align 8
%fptrToInt48454 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44395 to i64
%ae44395 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48454)
store volatile %struct.ScmObj* %ae44395, %struct.ScmObj** %stackaddr$makeclosure48453, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44395, %struct.ScmObj* %anf_45bind40375, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44395, %struct.ScmObj* %anf_45bind40370, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44395, %struct.ScmObj* %anf_45bind40369, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44395, %struct.ScmObj* %_37foldl40206, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44395, %struct.ScmObj* %anf_45bind40378, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44395, %struct.ScmObj* %anf_45bind40361, i64 5)
%ae44396 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48455 = alloca %struct.ScmObj*, align 8
%fptrToInt48456 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44397 to i64
%ae44397 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48456)
store volatile %struct.ScmObj* %ae44397, %struct.ScmObj** %stackaddr$makeclosure48455, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44397, %struct.ScmObj* %_37foldr140124, i64 0)
%args47350$ae44395$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48457 = alloca %struct.ScmObj*, align 8
%args47350$ae44395$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44397, %struct.ScmObj* %args47350$ae44395$0)
store volatile %struct.ScmObj* %args47350$ae44395$1, %struct.ScmObj** %stackaddr$prim48457, align 8
%stackaddr$prim48458 = alloca %struct.ScmObj*, align 8
%args47350$ae44395$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44396, %struct.ScmObj* %args47350$ae44395$1)
store volatile %struct.ScmObj* %args47350$ae44395$2, %struct.ScmObj** %stackaddr$prim48458, align 8
%clofunc48459 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44395)
musttail call tailcc void %clofunc48459(%struct.ScmObj* %ae44395, %struct.ScmObj* %args47350$ae44395$2)
ret void
}

define tailcc void @proc_clo$ae44395(%struct.ScmObj* %env$ae44395,%struct.ScmObj* %current_45args47332) {
%stackaddr$env-ref48460 = alloca %struct.ScmObj*, align 8
%anf_45bind40375 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44395, i64 0)
store %struct.ScmObj* %anf_45bind40375, %struct.ScmObj** %stackaddr$env-ref48460
%stackaddr$env-ref48461 = alloca %struct.ScmObj*, align 8
%anf_45bind40370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44395, i64 1)
store %struct.ScmObj* %anf_45bind40370, %struct.ScmObj** %stackaddr$env-ref48461
%stackaddr$env-ref48462 = alloca %struct.ScmObj*, align 8
%anf_45bind40369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44395, i64 2)
store %struct.ScmObj* %anf_45bind40369, %struct.ScmObj** %stackaddr$env-ref48462
%stackaddr$env-ref48463 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44395, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48463
%stackaddr$env-ref48464 = alloca %struct.ScmObj*, align 8
%anf_45bind40378 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44395, i64 4)
store %struct.ScmObj* %anf_45bind40378, %struct.ScmObj** %stackaddr$env-ref48464
%stackaddr$env-ref48465 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44395, i64 5)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48465
%stackaddr$prim48466 = alloca %struct.ScmObj*, align 8
%_95k40427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47332)
store volatile %struct.ScmObj* %_95k40427, %struct.ScmObj** %stackaddr$prim48466, align 8
%stackaddr$prim48467 = alloca %struct.ScmObj*, align 8
%current_45args47333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47332)
store volatile %struct.ScmObj* %current_45args47333, %struct.ScmObj** %stackaddr$prim48467, align 8
%stackaddr$prim48468 = alloca %struct.ScmObj*, align 8
%anf_45bind40380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47333)
store volatile %struct.ScmObj* %anf_45bind40380, %struct.ScmObj** %stackaddr$prim48468, align 8
%stackaddr$makeclosure48469 = alloca %struct.ScmObj*, align 8
%fptrToInt48470 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44466 to i64
%ae44466 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48470)
store volatile %struct.ScmObj* %ae44466, %struct.ScmObj** %stackaddr$makeclosure48469, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44466, %struct.ScmObj* %anf_45bind40369, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44466, %struct.ScmObj* %_37foldl40206, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44466, %struct.ScmObj* %anf_45bind40361, i64 2)
%args47343$anf_45bind40370$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48471 = alloca %struct.ScmObj*, align 8
%args47343$anf_45bind40370$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40380, %struct.ScmObj* %args47343$anf_45bind40370$0)
store volatile %struct.ScmObj* %args47343$anf_45bind40370$1, %struct.ScmObj** %stackaddr$prim48471, align 8
%stackaddr$prim48472 = alloca %struct.ScmObj*, align 8
%args47343$anf_45bind40370$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40378, %struct.ScmObj* %args47343$anf_45bind40370$1)
store volatile %struct.ScmObj* %args47343$anf_45bind40370$2, %struct.ScmObj** %stackaddr$prim48472, align 8
%stackaddr$prim48473 = alloca %struct.ScmObj*, align 8
%args47343$anf_45bind40370$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40375, %struct.ScmObj* %args47343$anf_45bind40370$2)
store volatile %struct.ScmObj* %args47343$anf_45bind40370$3, %struct.ScmObj** %stackaddr$prim48473, align 8
%stackaddr$prim48474 = alloca %struct.ScmObj*, align 8
%args47343$anf_45bind40370$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44466, %struct.ScmObj* %args47343$anf_45bind40370$3)
store volatile %struct.ScmObj* %args47343$anf_45bind40370$4, %struct.ScmObj** %stackaddr$prim48474, align 8
%clofunc48475 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40370)
musttail call tailcc void %clofunc48475(%struct.ScmObj* %anf_45bind40370, %struct.ScmObj* %args47343$anf_45bind40370$4)
ret void
}

define tailcc void @proc_clo$ae44466(%struct.ScmObj* %env$ae44466,%struct.ScmObj* %current_45args47335) {
%stackaddr$env-ref48476 = alloca %struct.ScmObj*, align 8
%anf_45bind40369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44466, i64 0)
store %struct.ScmObj* %anf_45bind40369, %struct.ScmObj** %stackaddr$env-ref48476
%stackaddr$env-ref48477 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44466, i64 1)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48477
%stackaddr$env-ref48478 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44466, i64 2)
store %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$env-ref48478
%stackaddr$prim48479 = alloca %struct.ScmObj*, align 8
%_95k40428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47335)
store volatile %struct.ScmObj* %_95k40428, %struct.ScmObj** %stackaddr$prim48479, align 8
%stackaddr$prim48480 = alloca %struct.ScmObj*, align 8
%current_45args47336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47335)
store volatile %struct.ScmObj* %current_45args47336, %struct.ScmObj** %stackaddr$prim48480, align 8
%stackaddr$prim48481 = alloca %struct.ScmObj*, align 8
%anf_45bind40381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47336)
store volatile %struct.ScmObj* %anf_45bind40381, %struct.ScmObj** %stackaddr$prim48481, align 8
%stackaddr$makeclosure48482 = alloca %struct.ScmObj*, align 8
%fptrToInt48483 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44471 to i64
%ae44471 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48483)
store volatile %struct.ScmObj* %ae44471, %struct.ScmObj** %stackaddr$makeclosure48482, align 8
%ae44473 = call %struct.ScmObj* @const_init_int(i64 0)
%args47342$_37foldl40206$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48484 = alloca %struct.ScmObj*, align 8
%args47342$_37foldl40206$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40381, %struct.ScmObj* %args47342$_37foldl40206$0)
store volatile %struct.ScmObj* %args47342$_37foldl40206$1, %struct.ScmObj** %stackaddr$prim48484, align 8
%stackaddr$prim48485 = alloca %struct.ScmObj*, align 8
%args47342$_37foldl40206$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40369, %struct.ScmObj* %args47342$_37foldl40206$1)
store volatile %struct.ScmObj* %args47342$_37foldl40206$2, %struct.ScmObj** %stackaddr$prim48485, align 8
%stackaddr$prim48486 = alloca %struct.ScmObj*, align 8
%args47342$_37foldl40206$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44473, %struct.ScmObj* %args47342$_37foldl40206$2)
store volatile %struct.ScmObj* %args47342$_37foldl40206$3, %struct.ScmObj** %stackaddr$prim48486, align 8
%stackaddr$prim48487 = alloca %struct.ScmObj*, align 8
%args47342$_37foldl40206$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40361, %struct.ScmObj* %args47342$_37foldl40206$3)
store volatile %struct.ScmObj* %args47342$_37foldl40206$4, %struct.ScmObj** %stackaddr$prim48487, align 8
%stackaddr$prim48488 = alloca %struct.ScmObj*, align 8
%args47342$_37foldl40206$5 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44471, %struct.ScmObj* %args47342$_37foldl40206$4)
store volatile %struct.ScmObj* %args47342$_37foldl40206$5, %struct.ScmObj** %stackaddr$prim48488, align 8
%clofunc48489 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40206)
musttail call tailcc void %clofunc48489(%struct.ScmObj* %_37foldl40206, %struct.ScmObj* %args47342$_37foldl40206$5)
ret void
}

define tailcc void @proc_clo$ae44471(%struct.ScmObj* %env$ae44471,%struct.ScmObj* %current_45args47338) {
%stackaddr$prim48490 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47338)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim48490, align 8
%stackaddr$prim48491 = alloca %struct.ScmObj*, align 8
%current_45args47339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47338)
store volatile %struct.ScmObj* %current_45args47339, %struct.ScmObj** %stackaddr$prim48491, align 8
%stackaddr$prim48492 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47339)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim48492, align 8
%stackaddr$prim48493 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim48493, align 8
%args47341$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48494 = alloca %struct.ScmObj*, align 8
%args47341$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args47341$k$0)
store volatile %struct.ScmObj* %args47341$k$1, %struct.ScmObj** %stackaddr$prim48494, align 8
%clofunc48495 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc48495(%struct.ScmObj* %k, %struct.ScmObj* %args47341$k$1)
ret void
}

define tailcc void @proc_clo$ae44397(%struct.ScmObj* %env$ae44397,%struct.ScmObj* %a4024140429) {
%stackaddr$env-ref48496 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44397, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48496
%stackaddr$prim48497 = alloca %struct.ScmObj*, align 8
%k40430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %a4024140429)
store volatile %struct.ScmObj* %k40430, %struct.ScmObj** %stackaddr$prim48497, align 8
%stackaddr$prim48498 = alloca %struct.ScmObj*, align 8
%a40241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %a4024140429)
store volatile %struct.ScmObj* %a40241, %struct.ScmObj** %stackaddr$prim48498, align 8
%stackaddr$makeclosure48499 = alloca %struct.ScmObj*, align 8
%fptrToInt48500 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44400 to i64
%ae44400 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48500)
store volatile %struct.ScmObj* %ae44400, %struct.ScmObj** %stackaddr$makeclosure48499, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44400, %struct.ScmObj* %a40241, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44400, %struct.ScmObj* %k40430, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44400, %struct.ScmObj* %_37foldr140124, i64 2)
%ae44401 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48501 = alloca %struct.ScmObj*, align 8
%fptrToInt48502 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44402 to i64
%ae44402 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48502)
store volatile %struct.ScmObj* %ae44402, %struct.ScmObj** %stackaddr$makeclosure48501, align 8
%args47349$ae44400$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48503 = alloca %struct.ScmObj*, align 8
%args47349$ae44400$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44402, %struct.ScmObj* %args47349$ae44400$0)
store volatile %struct.ScmObj* %args47349$ae44400$1, %struct.ScmObj** %stackaddr$prim48503, align 8
%stackaddr$prim48504 = alloca %struct.ScmObj*, align 8
%args47349$ae44400$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44401, %struct.ScmObj* %args47349$ae44400$1)
store volatile %struct.ScmObj* %args47349$ae44400$2, %struct.ScmObj** %stackaddr$prim48504, align 8
%clofunc48505 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44400)
musttail call tailcc void %clofunc48505(%struct.ScmObj* %ae44400, %struct.ScmObj* %args47349$ae44400$2)
ret void
}

define tailcc void @proc_clo$ae44400(%struct.ScmObj* %env$ae44400,%struct.ScmObj* %current_45args47344) {
%stackaddr$env-ref48506 = alloca %struct.ScmObj*, align 8
%a40241 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44400, i64 0)
store %struct.ScmObj* %a40241, %struct.ScmObj** %stackaddr$env-ref48506
%stackaddr$env-ref48507 = alloca %struct.ScmObj*, align 8
%k40430 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44400, i64 1)
store %struct.ScmObj* %k40430, %struct.ScmObj** %stackaddr$env-ref48507
%stackaddr$env-ref48508 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44400, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48508
%stackaddr$prim48509 = alloca %struct.ScmObj*, align 8
%_95k40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47344)
store volatile %struct.ScmObj* %_95k40431, %struct.ScmObj** %stackaddr$prim48509, align 8
%stackaddr$prim48510 = alloca %struct.ScmObj*, align 8
%current_45args47345 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47344)
store volatile %struct.ScmObj* %current_45args47345, %struct.ScmObj** %stackaddr$prim48510, align 8
%stackaddr$prim48511 = alloca %struct.ScmObj*, align 8
%anf_45bind40379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47345)
store volatile %struct.ScmObj* %anf_45bind40379, %struct.ScmObj** %stackaddr$prim48511, align 8
%ae44427 = call %struct.ScmObj* @const_init_int(i64 0)
%args47347$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48512 = alloca %struct.ScmObj*, align 8
%args47347$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40241, %struct.ScmObj* %args47347$_37foldr140124$0)
store volatile %struct.ScmObj* %args47347$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim48512, align 8
%stackaddr$prim48513 = alloca %struct.ScmObj*, align 8
%args47347$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44427, %struct.ScmObj* %args47347$_37foldr140124$1)
store volatile %struct.ScmObj* %args47347$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim48513, align 8
%stackaddr$prim48514 = alloca %struct.ScmObj*, align 8
%args47347$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40379, %struct.ScmObj* %args47347$_37foldr140124$2)
store volatile %struct.ScmObj* %args47347$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim48514, align 8
%stackaddr$prim48515 = alloca %struct.ScmObj*, align 8
%args47347$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40430, %struct.ScmObj* %args47347$_37foldr140124$3)
store volatile %struct.ScmObj* %args47347$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim48515, align 8
%clofunc48516 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc48516(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args47347$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae44402(%struct.ScmObj* %env$ae44402,%struct.ScmObj* %args4024240432) {
%stackaddr$prim48517 = alloca %struct.ScmObj*, align 8
%k40433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4024240432)
store volatile %struct.ScmObj* %k40433, %struct.ScmObj** %stackaddr$prim48517, align 8
%stackaddr$prim48518 = alloca %struct.ScmObj*, align 8
%args40242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4024240432)
store volatile %struct.ScmObj* %args40242, %struct.ScmObj** %stackaddr$prim48518, align 8
%stackaddr$applyprim48519 = alloca %struct.ScmObj*, align 8
%cpsaprim40434 = call %struct.ScmObj* @applyprim__43(%struct.ScmObj* %args40242)
store volatile %struct.ScmObj* %cpsaprim40434, %struct.ScmObj** %stackaddr$applyprim48519, align 8
%ae44407 = call %struct.ScmObj* @const_init_int(i64 0)
%args47348$k40433$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48520 = alloca %struct.ScmObj*, align 8
%args47348$k40433$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim40434, %struct.ScmObj* %args47348$k40433$0)
store volatile %struct.ScmObj* %args47348$k40433$1, %struct.ScmObj** %stackaddr$prim48520, align 8
%stackaddr$prim48521 = alloca %struct.ScmObj*, align 8
%args47348$k40433$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44407, %struct.ScmObj* %args47348$k40433$1)
store volatile %struct.ScmObj* %args47348$k40433$2, %struct.ScmObj** %stackaddr$prim48521, align 8
%clofunc48522 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40433)
musttail call tailcc void %clofunc48522(%struct.ScmObj* %k40433, %struct.ScmObj* %args47348$k40433$2)
ret void
}

define tailcc void @proc_clo$ae44367(%struct.ScmObj* %env$ae44367,%struct.ScmObj* %current_45args47351) {
%stackaddr$prim48523 = alloca %struct.ScmObj*, align 8
%k40435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47351)
store volatile %struct.ScmObj* %k40435, %struct.ScmObj** %stackaddr$prim48523, align 8
%stackaddr$prim48524 = alloca %struct.ScmObj*, align 8
%current_45args47352 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47351)
store volatile %struct.ScmObj* %current_45args47352, %struct.ScmObj** %stackaddr$prim48524, align 8
%stackaddr$prim48525 = alloca %struct.ScmObj*, align 8
%a40240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47352)
store volatile %struct.ScmObj* %a40240, %struct.ScmObj** %stackaddr$prim48525, align 8
%stackaddr$prim48526 = alloca %struct.ScmObj*, align 8
%current_45args47353 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47352)
store volatile %struct.ScmObj* %current_45args47353, %struct.ScmObj** %stackaddr$prim48526, align 8
%stackaddr$prim48527 = alloca %struct.ScmObj*, align 8
%b40239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47353)
store volatile %struct.ScmObj* %b40239, %struct.ScmObj** %stackaddr$prim48527, align 8
%stackaddr$prim48528 = alloca %struct.ScmObj*, align 8
%current_45args47354 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47353)
store volatile %struct.ScmObj* %current_45args47354, %struct.ScmObj** %stackaddr$prim48528, align 8
%stackaddr$prim48529 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47354)
store volatile %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$prim48529, align 8
%stackaddr$prim48530 = alloca %struct.ScmObj*, align 8
%current_45args47355 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47354)
store volatile %struct.ScmObj* %current_45args47355, %struct.ScmObj** %stackaddr$prim48530, align 8
%stackaddr$prim48531 = alloca %struct.ScmObj*, align 8
%d40237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47355)
store volatile %struct.ScmObj* %d40237, %struct.ScmObj** %stackaddr$prim48531, align 8
%stackaddr$prim48532 = alloca %struct.ScmObj*, align 8
%anf_45bind40376 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %a40240, %struct.ScmObj* %b40239)
store volatile %struct.ScmObj* %anf_45bind40376, %struct.ScmObj** %stackaddr$prim48532, align 8
%stackaddr$prim48533 = alloca %struct.ScmObj*, align 8
%anf_45bind40377 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %c40238, %struct.ScmObj* %d40237)
store volatile %struct.ScmObj* %anf_45bind40377, %struct.ScmObj** %stackaddr$prim48533, align 8
%stackaddr$prim48534 = alloca %struct.ScmObj*, align 8
%cpsprim40436 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind40376, %struct.ScmObj* %anf_45bind40377)
store volatile %struct.ScmObj* %cpsprim40436, %struct.ScmObj** %stackaddr$prim48534, align 8
%ae44375 = call %struct.ScmObj* @const_init_int(i64 0)
%args47357$k40435$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48535 = alloca %struct.ScmObj*, align 8
%args47357$k40435$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40436, %struct.ScmObj* %args47357$k40435$0)
store volatile %struct.ScmObj* %args47357$k40435$1, %struct.ScmObj** %stackaddr$prim48535, align 8
%stackaddr$prim48536 = alloca %struct.ScmObj*, align 8
%args47357$k40435$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44375, %struct.ScmObj* %args47357$k40435$1)
store volatile %struct.ScmObj* %args47357$k40435$2, %struct.ScmObj** %stackaddr$prim48536, align 8
%clofunc48537 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40435)
musttail call tailcc void %clofunc48537(%struct.ScmObj* %k40435, %struct.ScmObj* %args47357$k40435$2)
ret void
}

define tailcc void @proc_clo$ae44295(%struct.ScmObj* %env$ae44295,%struct.ScmObj* %t401024023140437) {
%stackaddr$env-ref48538 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44295, i64 0)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48538
%stackaddr$env-ref48539 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44295, i64 1)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48539
%stackaddr$prim48540 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t401024023140437)
store volatile %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$prim48540, align 8
%stackaddr$prim48541 = alloca %struct.ScmObj*, align 8
%t4010240231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t401024023140437)
store volatile %struct.ScmObj* %t4010240231, %struct.ScmObj** %stackaddr$prim48541, align 8
%stackaddr$prim48542 = alloca %struct.ScmObj*, align 8
%a40232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t4010240231)
store volatile %struct.ScmObj* %a40232, %struct.ScmObj** %stackaddr$prim48542, align 8
%stackaddr$prim48543 = alloca %struct.ScmObj*, align 8
%t4010240233 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t4010240231)
store volatile %struct.ScmObj* %t4010240233, %struct.ScmObj** %stackaddr$prim48543, align 8
%stackaddr$prim48544 = alloca %struct.ScmObj*, align 8
%b40234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t4010240233)
store volatile %struct.ScmObj* %b40234, %struct.ScmObj** %stackaddr$prim48544, align 8
%stackaddr$prim48545 = alloca %struct.ScmObj*, align 8
%t4010240235 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t4010240233)
store volatile %struct.ScmObj* %t4010240235, %struct.ScmObj** %stackaddr$prim48545, align 8
%stackaddr$makeclosure48546 = alloca %struct.ScmObj*, align 8
%fptrToInt48547 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44302 to i64
%ae44302 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48547)
store volatile %struct.ScmObj* %ae44302, %struct.ScmObj** %stackaddr$makeclosure48546, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44302, %struct.ScmObj* %k40438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44302, %struct.ScmObj* %_37first40166, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44302, %struct.ScmObj* %_37second40164, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44302, %struct.ScmObj* %b40234, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44302, %struct.ScmObj* %a40232, i64 4)
%ae44303 = call %struct.ScmObj* @const_init_int(i64 0)
%args47371$ae44302$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48548 = alloca %struct.ScmObj*, align 8
%args47371$ae44302$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %t4010240235, %struct.ScmObj* %args47371$ae44302$0)
store volatile %struct.ScmObj* %args47371$ae44302$1, %struct.ScmObj** %stackaddr$prim48548, align 8
%stackaddr$prim48549 = alloca %struct.ScmObj*, align 8
%args47371$ae44302$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44303, %struct.ScmObj* %args47371$ae44302$1)
store volatile %struct.ScmObj* %args47371$ae44302$2, %struct.ScmObj** %stackaddr$prim48549, align 8
%clofunc48550 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44302)
musttail call tailcc void %clofunc48550(%struct.ScmObj* %ae44302, %struct.ScmObj* %args47371$ae44302$2)
ret void
}

define tailcc void @proc_clo$ae44302(%struct.ScmObj* %env$ae44302,%struct.ScmObj* %current_45args47359) {
%stackaddr$env-ref48551 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44302, i64 0)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref48551
%stackaddr$env-ref48552 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44302, i64 1)
store %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$env-ref48552
%stackaddr$env-ref48553 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44302, i64 2)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48553
%stackaddr$env-ref48554 = alloca %struct.ScmObj*, align 8
%b40234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44302, i64 3)
store %struct.ScmObj* %b40234, %struct.ScmObj** %stackaddr$env-ref48554
%stackaddr$env-ref48555 = alloca %struct.ScmObj*, align 8
%a40232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44302, i64 4)
store %struct.ScmObj* %a40232, %struct.ScmObj** %stackaddr$env-ref48555
%stackaddr$prim48556 = alloca %struct.ScmObj*, align 8
%_95k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47359)
store volatile %struct.ScmObj* %_95k40439, %struct.ScmObj** %stackaddr$prim48556, align 8
%stackaddr$prim48557 = alloca %struct.ScmObj*, align 8
%current_45args47360 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47359)
store volatile %struct.ScmObj* %current_45args47360, %struct.ScmObj** %stackaddr$prim48557, align 8
%stackaddr$prim48558 = alloca %struct.ScmObj*, align 8
%c40236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47360)
store volatile %struct.ScmObj* %c40236, %struct.ScmObj** %stackaddr$prim48558, align 8
%stackaddr$makeclosure48559 = alloca %struct.ScmObj*, align 8
%fptrToInt48560 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44309 to i64
%ae44309 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48560)
store volatile %struct.ScmObj* %ae44309, %struct.ScmObj** %stackaddr$makeclosure48559, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44309, %struct.ScmObj* %k40438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44309, %struct.ScmObj* %_37second40164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44309, %struct.ScmObj* %c40236, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44309, %struct.ScmObj* %b40234, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44309, %struct.ScmObj* %a40232, i64 4)
%args47370$_37first40166$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48561 = alloca %struct.ScmObj*, align 8
%args47370$_37first40166$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c40236, %struct.ScmObj* %args47370$_37first40166$0)
store volatile %struct.ScmObj* %args47370$_37first40166$1, %struct.ScmObj** %stackaddr$prim48561, align 8
%stackaddr$prim48562 = alloca %struct.ScmObj*, align 8
%args47370$_37first40166$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44309, %struct.ScmObj* %args47370$_37first40166$1)
store volatile %struct.ScmObj* %args47370$_37first40166$2, %struct.ScmObj** %stackaddr$prim48562, align 8
%clofunc48563 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37first40166)
musttail call tailcc void %clofunc48563(%struct.ScmObj* %_37first40166, %struct.ScmObj* %args47370$_37first40166$2)
ret void
}

define tailcc void @proc_clo$ae44309(%struct.ScmObj* %env$ae44309,%struct.ScmObj* %current_45args47362) {
%stackaddr$env-ref48564 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44309, i64 0)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref48564
%stackaddr$env-ref48565 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44309, i64 1)
store %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$env-ref48565
%stackaddr$env-ref48566 = alloca %struct.ScmObj*, align 8
%c40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44309, i64 2)
store %struct.ScmObj* %c40236, %struct.ScmObj** %stackaddr$env-ref48566
%stackaddr$env-ref48567 = alloca %struct.ScmObj*, align 8
%b40234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44309, i64 3)
store %struct.ScmObj* %b40234, %struct.ScmObj** %stackaddr$env-ref48567
%stackaddr$env-ref48568 = alloca %struct.ScmObj*, align 8
%a40232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44309, i64 4)
store %struct.ScmObj* %a40232, %struct.ScmObj** %stackaddr$env-ref48568
%stackaddr$prim48569 = alloca %struct.ScmObj*, align 8
%_95k40440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47362)
store volatile %struct.ScmObj* %_95k40440, %struct.ScmObj** %stackaddr$prim48569, align 8
%stackaddr$prim48570 = alloca %struct.ScmObj*, align 8
%current_45args47363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47362)
store volatile %struct.ScmObj* %current_45args47363, %struct.ScmObj** %stackaddr$prim48570, align 8
%stackaddr$prim48571 = alloca %struct.ScmObj*, align 8
%anf_45bind40371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47363)
store volatile %struct.ScmObj* %anf_45bind40371, %struct.ScmObj** %stackaddr$prim48571, align 8
%stackaddr$makeclosure48572 = alloca %struct.ScmObj*, align 8
%fptrToInt48573 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44312 to i64
%ae44312 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48573)
store volatile %struct.ScmObj* %ae44312, %struct.ScmObj** %stackaddr$makeclosure48572, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44312, %struct.ScmObj* %k40438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44312, %struct.ScmObj* %anf_45bind40371, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44312, %struct.ScmObj* %b40234, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44312, %struct.ScmObj* %a40232, i64 3)
%args47369$_37second40164$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48574 = alloca %struct.ScmObj*, align 8
%args47369$_37second40164$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c40236, %struct.ScmObj* %args47369$_37second40164$0)
store volatile %struct.ScmObj* %args47369$_37second40164$1, %struct.ScmObj** %stackaddr$prim48574, align 8
%stackaddr$prim48575 = alloca %struct.ScmObj*, align 8
%args47369$_37second40164$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44312, %struct.ScmObj* %args47369$_37second40164$1)
store volatile %struct.ScmObj* %args47369$_37second40164$2, %struct.ScmObj** %stackaddr$prim48575, align 8
%clofunc48576 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37second40164)
musttail call tailcc void %clofunc48576(%struct.ScmObj* %_37second40164, %struct.ScmObj* %args47369$_37second40164$2)
ret void
}

define tailcc void @proc_clo$ae44312(%struct.ScmObj* %env$ae44312,%struct.ScmObj* %current_45args47365) {
%stackaddr$env-ref48577 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44312, i64 0)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref48577
%stackaddr$env-ref48578 = alloca %struct.ScmObj*, align 8
%anf_45bind40371 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44312, i64 1)
store %struct.ScmObj* %anf_45bind40371, %struct.ScmObj** %stackaddr$env-ref48578
%stackaddr$env-ref48579 = alloca %struct.ScmObj*, align 8
%b40234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44312, i64 2)
store %struct.ScmObj* %b40234, %struct.ScmObj** %stackaddr$env-ref48579
%stackaddr$env-ref48580 = alloca %struct.ScmObj*, align 8
%a40232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44312, i64 3)
store %struct.ScmObj* %a40232, %struct.ScmObj** %stackaddr$env-ref48580
%stackaddr$prim48581 = alloca %struct.ScmObj*, align 8
%_95k40441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47365)
store volatile %struct.ScmObj* %_95k40441, %struct.ScmObj** %stackaddr$prim48581, align 8
%stackaddr$prim48582 = alloca %struct.ScmObj*, align 8
%current_45args47366 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47365)
store volatile %struct.ScmObj* %current_45args47366, %struct.ScmObj** %stackaddr$prim48582, align 8
%stackaddr$prim48583 = alloca %struct.ScmObj*, align 8
%anf_45bind40372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47366)
store volatile %struct.ScmObj* %anf_45bind40372, %struct.ScmObj** %stackaddr$prim48583, align 8
%stackaddr$prim48584 = alloca %struct.ScmObj*, align 8
%anf_45bind40373 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind40371, %struct.ScmObj* %anf_45bind40372)
store volatile %struct.ScmObj* %anf_45bind40373, %struct.ScmObj** %stackaddr$prim48584, align 8
%stackaddr$prim48585 = alloca %struct.ScmObj*, align 8
%anf_45bind40374 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %b40234, %struct.ScmObj* %anf_45bind40373)
store volatile %struct.ScmObj* %anf_45bind40374, %struct.ScmObj** %stackaddr$prim48585, align 8
%stackaddr$prim48586 = alloca %struct.ScmObj*, align 8
%cpsprim40442 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %a40232, %struct.ScmObj* %anf_45bind40374)
store volatile %struct.ScmObj* %cpsprim40442, %struct.ScmObj** %stackaddr$prim48586, align 8
%ae44321 = call %struct.ScmObj* @const_init_int(i64 0)
%args47368$k40438$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48587 = alloca %struct.ScmObj*, align 8
%args47368$k40438$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40442, %struct.ScmObj* %args47368$k40438$0)
store volatile %struct.ScmObj* %args47368$k40438$1, %struct.ScmObj** %stackaddr$prim48587, align 8
%stackaddr$prim48588 = alloca %struct.ScmObj*, align 8
%args47368$k40438$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44321, %struct.ScmObj* %args47368$k40438$1)
store volatile %struct.ScmObj* %args47368$k40438$2, %struct.ScmObj** %stackaddr$prim48588, align 8
%clofunc48589 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40438)
musttail call tailcc void %clofunc48589(%struct.ScmObj* %k40438, %struct.ScmObj* %args47368$k40438$2)
ret void
}

define tailcc void @proc_clo$ae44273(%struct.ScmObj* %env$ae44273,%struct.ScmObj* %a4023040443) {
%stackaddr$prim48590 = alloca %struct.ScmObj*, align 8
%k40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %a4023040443)
store volatile %struct.ScmObj* %k40444, %struct.ScmObj** %stackaddr$prim48590, align 8
%stackaddr$prim48591 = alloca %struct.ScmObj*, align 8
%a40230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %a4023040443)
store volatile %struct.ScmObj* %a40230, %struct.ScmObj** %stackaddr$prim48591, align 8
%ae44277 = call %struct.ScmObj* @const_init_int(i64 0)
%args47373$k40444$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48592 = alloca %struct.ScmObj*, align 8
%args47373$k40444$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40230, %struct.ScmObj* %args47373$k40444$0)
store volatile %struct.ScmObj* %args47373$k40444$1, %struct.ScmObj** %stackaddr$prim48592, align 8
%stackaddr$prim48593 = alloca %struct.ScmObj*, align 8
%args47373$k40444$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44277, %struct.ScmObj* %args47373$k40444$1)
store volatile %struct.ScmObj* %args47373$k40444$2, %struct.ScmObj** %stackaddr$prim48593, align 8
%clofunc48594 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40444)
musttail call tailcc void %clofunc48594(%struct.ScmObj* %k40444, %struct.ScmObj* %args47373$k40444$2)
ret void
}

define tailcc void @proc_clo$ae44216(%struct.ScmObj* %env$ae44216,%struct.ScmObj* %lst4022940445) {
%stackaddr$prim48595 = alloca %struct.ScmObj*, align 8
%k40446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022940445)
store volatile %struct.ScmObj* %k40446, %struct.ScmObj** %stackaddr$prim48595, align 8
%stackaddr$prim48596 = alloca %struct.ScmObj*, align 8
%lst40229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022940445)
store volatile %struct.ScmObj* %lst40229, %struct.ScmObj** %stackaddr$prim48596, align 8
%ae44220 = call %struct.ScmObj* @const_init_int(i64 0)
%args47377$k40446$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48597 = alloca %struct.ScmObj*, align 8
%args47377$k40446$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40229, %struct.ScmObj* %args47377$k40446$0)
store volatile %struct.ScmObj* %args47377$k40446$1, %struct.ScmObj** %stackaddr$prim48597, align 8
%stackaddr$prim48598 = alloca %struct.ScmObj*, align 8
%args47377$k40446$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44220, %struct.ScmObj* %args47377$k40446$1)
store volatile %struct.ScmObj* %args47377$k40446$2, %struct.ScmObj** %stackaddr$prim48598, align 8
%clofunc48599 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40446)
musttail call tailcc void %clofunc48599(%struct.ScmObj* %k40446, %struct.ScmObj* %args47377$k40446$2)
ret void
}

define tailcc void @proc_clo$ae44164(%struct.ScmObj* %env$ae44164,%struct.ScmObj* %lst4022840447) {
%stackaddr$prim48600 = alloca %struct.ScmObj*, align 8
%k40448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022840447)
store volatile %struct.ScmObj* %k40448, %struct.ScmObj** %stackaddr$prim48600, align 8
%stackaddr$prim48601 = alloca %struct.ScmObj*, align 8
%lst40228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022840447)
store volatile %struct.ScmObj* %lst40228, %struct.ScmObj** %stackaddr$prim48601, align 8
%ae44168 = call %struct.ScmObj* @const_init_int(i64 0)
%args47380$k40448$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48602 = alloca %struct.ScmObj*, align 8
%args47380$k40448$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40228, %struct.ScmObj* %args47380$k40448$0)
store volatile %struct.ScmObj* %args47380$k40448$1, %struct.ScmObj** %stackaddr$prim48602, align 8
%stackaddr$prim48603 = alloca %struct.ScmObj*, align 8
%args47380$k40448$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44168, %struct.ScmObj* %args47380$k40448$1)
store volatile %struct.ScmObj* %args47380$k40448$2, %struct.ScmObj** %stackaddr$prim48603, align 8
%clofunc48604 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40448)
musttail call tailcc void %clofunc48604(%struct.ScmObj* %k40448, %struct.ScmObj* %args47380$k40448$2)
ret void
}

define tailcc void @proc_clo$ae44112(%struct.ScmObj* %env$ae44112,%struct.ScmObj* %lst4022740449) {
%stackaddr$prim48605 = alloca %struct.ScmObj*, align 8
%k40450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022740449)
store volatile %struct.ScmObj* %k40450, %struct.ScmObj** %stackaddr$prim48605, align 8
%stackaddr$prim48606 = alloca %struct.ScmObj*, align 8
%lst40227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022740449)
store volatile %struct.ScmObj* %lst40227, %struct.ScmObj** %stackaddr$prim48606, align 8
%ae44116 = call %struct.ScmObj* @const_init_int(i64 0)
%args47383$k40450$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48607 = alloca %struct.ScmObj*, align 8
%args47383$k40450$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40227, %struct.ScmObj* %args47383$k40450$0)
store volatile %struct.ScmObj* %args47383$k40450$1, %struct.ScmObj** %stackaddr$prim48607, align 8
%stackaddr$prim48608 = alloca %struct.ScmObj*, align 8
%args47383$k40450$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44116, %struct.ScmObj* %args47383$k40450$1)
store volatile %struct.ScmObj* %args47383$k40450$2, %struct.ScmObj** %stackaddr$prim48608, align 8
%clofunc48609 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40450)
musttail call tailcc void %clofunc48609(%struct.ScmObj* %k40450, %struct.ScmObj* %args47383$k40450$2)
ret void
}

define tailcc void @proc_clo$ae44090(%struct.ScmObj* %env$ae44090,%struct.ScmObj* %lst4022640451) {
%stackaddr$prim48610 = alloca %struct.ScmObj*, align 8
%k40452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022640451)
store volatile %struct.ScmObj* %k40452, %struct.ScmObj** %stackaddr$prim48610, align 8
%stackaddr$prim48611 = alloca %struct.ScmObj*, align 8
%lst40226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022640451)
store volatile %struct.ScmObj* %lst40226, %struct.ScmObj** %stackaddr$prim48611, align 8
%ae44094 = call %struct.ScmObj* @const_init_int(i64 0)
%args47385$k40452$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48612 = alloca %struct.ScmObj*, align 8
%args47385$k40452$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40226, %struct.ScmObj* %args47385$k40452$0)
store volatile %struct.ScmObj* %args47385$k40452$1, %struct.ScmObj** %stackaddr$prim48612, align 8
%stackaddr$prim48613 = alloca %struct.ScmObj*, align 8
%args47385$k40452$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44094, %struct.ScmObj* %args47385$k40452$1)
store volatile %struct.ScmObj* %args47385$k40452$2, %struct.ScmObj** %stackaddr$prim48613, align 8
%clofunc48614 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40452)
musttail call tailcc void %clofunc48614(%struct.ScmObj* %k40452, %struct.ScmObj* %args47385$k40452$2)
ret void
}

define tailcc void @proc_clo$ae44052(%struct.ScmObj* %env$ae44052,%struct.ScmObj* %current_45args47387) {
%stackaddr$prim48615 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47387)
store volatile %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$prim48615, align 8
%stackaddr$prim48616 = alloca %struct.ScmObj*, align 8
%current_45args47388 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47387)
store volatile %struct.ScmObj* %current_45args47388, %struct.ScmObj** %stackaddr$prim48616, align 8
%stackaddr$prim48617 = alloca %struct.ScmObj*, align 8
%lst40225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47388)
store volatile %struct.ScmObj* %lst40225, %struct.ScmObj** %stackaddr$prim48617, align 8
%stackaddr$prim48618 = alloca %struct.ScmObj*, align 8
%current_45args47389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47388)
store volatile %struct.ScmObj* %current_45args47389, %struct.ScmObj** %stackaddr$prim48618, align 8
%stackaddr$prim48619 = alloca %struct.ScmObj*, align 8
%f40224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47389)
store volatile %struct.ScmObj* %f40224, %struct.ScmObj** %stackaddr$prim48619, align 8
%stackaddr$prim48620 = alloca %struct.ScmObj*, align 8
%current_45args47390 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47389)
store volatile %struct.ScmObj* %current_45args47390, %struct.ScmObj** %stackaddr$prim48620, align 8
%stackaddr$prim48621 = alloca %struct.ScmObj*, align 8
%a40223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47390)
store volatile %struct.ScmObj* %a40223, %struct.ScmObj** %stackaddr$prim48621, align 8
%stackaddr$makeclosure48622 = alloca %struct.ScmObj*, align 8
%fptrToInt48623 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44053 to i64
%ae44053 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48623)
store volatile %struct.ScmObj* %ae44053, %struct.ScmObj** %stackaddr$makeclosure48622, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44053, %struct.ScmObj* %k40453, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44053, %struct.ScmObj* %lst40225, i64 1)
%stackaddr$prim48624 = alloca %struct.ScmObj*, align 8
%cpsargs40456 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44053, %struct.ScmObj* %lst40225)
store volatile %struct.ScmObj* %cpsargs40456, %struct.ScmObj** %stackaddr$prim48624, align 8
%clofunc48625 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40224)
musttail call tailcc void %clofunc48625(%struct.ScmObj* %f40224, %struct.ScmObj* %cpsargs40456)
ret void
}

define tailcc void @proc_clo$ae44053(%struct.ScmObj* %env$ae44053,%struct.ScmObj* %current_45args47392) {
%stackaddr$env-ref48626 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44053, i64 0)
store %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$env-ref48626
%stackaddr$env-ref48627 = alloca %struct.ScmObj*, align 8
%lst40225 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44053, i64 1)
store %struct.ScmObj* %lst40225, %struct.ScmObj** %stackaddr$env-ref48627
%stackaddr$prim48628 = alloca %struct.ScmObj*, align 8
%_95k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47392)
store volatile %struct.ScmObj* %_95k40454, %struct.ScmObj** %stackaddr$prim48628, align 8
%stackaddr$prim48629 = alloca %struct.ScmObj*, align 8
%current_45args47393 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47392)
store volatile %struct.ScmObj* %current_45args47393, %struct.ScmObj** %stackaddr$prim48629, align 8
%stackaddr$prim48630 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47393)
store volatile %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$prim48630, align 8
%stackaddr$prim48631 = alloca %struct.ScmObj*, align 8
%anf_45bind40359 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40225)
store volatile %struct.ScmObj* %anf_45bind40359, %struct.ScmObj** %stackaddr$prim48631, align 8
%stackaddr$prim48632 = alloca %struct.ScmObj*, align 8
%anf_45bind40360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40359)
store volatile %struct.ScmObj* %anf_45bind40360, %struct.ScmObj** %stackaddr$prim48632, align 8
%stackaddr$prim48633 = alloca %struct.ScmObj*, align 8
%cpsprim40455 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind40358, %struct.ScmObj* %anf_45bind40360)
store volatile %struct.ScmObj* %cpsprim40455, %struct.ScmObj** %stackaddr$prim48633, align 8
%ae44062 = call %struct.ScmObj* @const_init_int(i64 0)
%args47395$k40453$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48634 = alloca %struct.ScmObj*, align 8
%args47395$k40453$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40455, %struct.ScmObj* %args47395$k40453$0)
store volatile %struct.ScmObj* %args47395$k40453$1, %struct.ScmObj** %stackaddr$prim48634, align 8
%stackaddr$prim48635 = alloca %struct.ScmObj*, align 8
%args47395$k40453$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44062, %struct.ScmObj* %args47395$k40453$1)
store volatile %struct.ScmObj* %args47395$k40453$2, %struct.ScmObj** %stackaddr$prim48635, align 8
%clofunc48636 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40453)
musttail call tailcc void %clofunc48636(%struct.ScmObj* %k40453, %struct.ScmObj* %args47395$k40453$2)
ret void
}

define tailcc void @proc_clo$ae43965(%struct.ScmObj* %env$ae43965,%struct.ScmObj* %current_45args47397) {
%stackaddr$prim48637 = alloca %struct.ScmObj*, align 8
%k40457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47397)
store volatile %struct.ScmObj* %k40457, %struct.ScmObj** %stackaddr$prim48637, align 8
%stackaddr$prim48638 = alloca %struct.ScmObj*, align 8
%current_45args47398 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47397)
store volatile %struct.ScmObj* %current_45args47398, %struct.ScmObj** %stackaddr$prim48638, align 8
%stackaddr$prim48639 = alloca %struct.ScmObj*, align 8
%thunk40222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47398)
store volatile %struct.ScmObj* %thunk40222, %struct.ScmObj** %stackaddr$prim48639, align 8
%stackaddr$prim48640 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40222)
store volatile %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$prim48640, align 8
%truthy$cmp48641 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40354)
%cmp$cmp48641 = icmp eq i64 %truthy$cmp48641, 1
br i1 %cmp$cmp48641, label %truebranch$cmp48641, label %falsebranch$cmp48641
truebranch$cmp48641:
%stackaddr$prim48642 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40222)
store volatile %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$prim48642, align 8
%ae43970 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim48643 = alloca %struct.ScmObj*, align 8
%anf_45bind40356 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40355, %struct.ScmObj* %ae43970)
store volatile %struct.ScmObj* %anf_45bind40356, %struct.ScmObj** %stackaddr$prim48643, align 8
%truthy$cmp48644 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40356)
%cmp$cmp48644 = icmp eq i64 %truthy$cmp48644, 1
br i1 %cmp$cmp48644, label %truebranch$cmp48644, label %falsebranch$cmp48644
truebranch$cmp48644:
%ae43973 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48645 = alloca %struct.ScmObj*, align 8
%anf_45bind40357 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40222, %struct.ScmObj* %ae43973)
store volatile %struct.ScmObj* %anf_45bind40357, %struct.ScmObj** %stackaddr$prim48645, align 8
%ae43975 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4397548646, i32 0, i32 0))
%stackaddr$prim48647 = alloca %struct.ScmObj*, align 8
%cpsprim40458 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40357, %struct.ScmObj* %ae43975)
store volatile %struct.ScmObj* %cpsprim40458, %struct.ScmObj** %stackaddr$prim48647, align 8
%ae43977 = call %struct.ScmObj* @const_init_int(i64 0)
%args47400$k40457$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48648 = alloca %struct.ScmObj*, align 8
%args47400$k40457$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40458, %struct.ScmObj* %args47400$k40457$0)
store volatile %struct.ScmObj* %args47400$k40457$1, %struct.ScmObj** %stackaddr$prim48648, align 8
%stackaddr$prim48649 = alloca %struct.ScmObj*, align 8
%args47400$k40457$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43977, %struct.ScmObj* %args47400$k40457$1)
store volatile %struct.ScmObj* %args47400$k40457$2, %struct.ScmObj** %stackaddr$prim48649, align 8
%clofunc48650 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40457)
musttail call tailcc void %clofunc48650(%struct.ScmObj* %k40457, %struct.ScmObj* %args47400$k40457$2)
ret void
falsebranch$cmp48644:
%ae43995 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43996 = call %struct.ScmObj* @const_init_false()
%args47401$k40457$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48651 = alloca %struct.ScmObj*, align 8
%args47401$k40457$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43996, %struct.ScmObj* %args47401$k40457$0)
store volatile %struct.ScmObj* %args47401$k40457$1, %struct.ScmObj** %stackaddr$prim48651, align 8
%stackaddr$prim48652 = alloca %struct.ScmObj*, align 8
%args47401$k40457$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43995, %struct.ScmObj* %args47401$k40457$1)
store volatile %struct.ScmObj* %args47401$k40457$2, %struct.ScmObj** %stackaddr$prim48652, align 8
%clofunc48653 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40457)
musttail call tailcc void %clofunc48653(%struct.ScmObj* %k40457, %struct.ScmObj* %args47401$k40457$2)
ret void
falsebranch$cmp48641:
%ae44017 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44018 = call %struct.ScmObj* @const_init_false()
%args47402$k40457$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48654 = alloca %struct.ScmObj*, align 8
%args47402$k40457$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44018, %struct.ScmObj* %args47402$k40457$0)
store volatile %struct.ScmObj* %args47402$k40457$1, %struct.ScmObj** %stackaddr$prim48654, align 8
%stackaddr$prim48655 = alloca %struct.ScmObj*, align 8
%args47402$k40457$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44017, %struct.ScmObj* %args47402$k40457$1)
store volatile %struct.ScmObj* %args47402$k40457$2, %struct.ScmObj** %stackaddr$prim48655, align 8
%clofunc48656 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40457)
musttail call tailcc void %clofunc48656(%struct.ScmObj* %k40457, %struct.ScmObj* %args47402$k40457$2)
ret void
}

define tailcc void @proc_clo$ae43939(%struct.ScmObj* %env$ae43939,%struct.ScmObj* %current_45args47404) {
%stackaddr$prim48657 = alloca %struct.ScmObj*, align 8
%k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47404)
store volatile %struct.ScmObj* %k40459, %struct.ScmObj** %stackaddr$prim48657, align 8
%stackaddr$prim48658 = alloca %struct.ScmObj*, align 8
%current_45args47405 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47404)
store volatile %struct.ScmObj* %current_45args47405, %struct.ScmObj** %stackaddr$prim48658, align 8
%stackaddr$prim48659 = alloca %struct.ScmObj*, align 8
%x40161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47405)
store volatile %struct.ScmObj* %x40161, %struct.ScmObj** %stackaddr$prim48659, align 8
%stackaddr$prim48660 = alloca %struct.ScmObj*, align 8
%anf_45bind40351 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40161)
store volatile %struct.ScmObj* %anf_45bind40351, %struct.ScmObj** %stackaddr$prim48660, align 8
%stackaddr$prim48661 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40351)
store volatile %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$prim48661, align 8
%stackaddr$prim48662 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40352)
store volatile %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$prim48662, align 8
%stackaddr$prim48663 = alloca %struct.ScmObj*, align 8
%cpsprim40460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40353)
store volatile %struct.ScmObj* %cpsprim40460, %struct.ScmObj** %stackaddr$prim48663, align 8
%ae43945 = call %struct.ScmObj* @const_init_int(i64 0)
%args47407$k40459$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48664 = alloca %struct.ScmObj*, align 8
%args47407$k40459$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40460, %struct.ScmObj* %args47407$k40459$0)
store volatile %struct.ScmObj* %args47407$k40459$1, %struct.ScmObj** %stackaddr$prim48664, align 8
%stackaddr$prim48665 = alloca %struct.ScmObj*, align 8
%args47407$k40459$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43945, %struct.ScmObj* %args47407$k40459$1)
store volatile %struct.ScmObj* %args47407$k40459$2, %struct.ScmObj** %stackaddr$prim48665, align 8
%clofunc48666 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40459)
musttail call tailcc void %clofunc48666(%struct.ScmObj* %k40459, %struct.ScmObj* %args47407$k40459$2)
ret void
}

define tailcc void @proc_clo$ae43915(%struct.ScmObj* %env$ae43915,%struct.ScmObj* %current_45args47409) {
%stackaddr$prim48667 = alloca %struct.ScmObj*, align 8
%k40461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47409)
store volatile %struct.ScmObj* %k40461, %struct.ScmObj** %stackaddr$prim48667, align 8
%stackaddr$prim48668 = alloca %struct.ScmObj*, align 8
%current_45args47410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47409)
store volatile %struct.ScmObj* %current_45args47410, %struct.ScmObj** %stackaddr$prim48668, align 8
%stackaddr$prim48669 = alloca %struct.ScmObj*, align 8
%x40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47410)
store volatile %struct.ScmObj* %x40163, %struct.ScmObj** %stackaddr$prim48669, align 8
%stackaddr$prim48670 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40163)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim48670, align 8
%stackaddr$prim48671 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40349)
store volatile %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$prim48671, align 8
%stackaddr$prim48672 = alloca %struct.ScmObj*, align 8
%cpsprim40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40350)
store volatile %struct.ScmObj* %cpsprim40462, %struct.ScmObj** %stackaddr$prim48672, align 8
%ae43920 = call %struct.ScmObj* @const_init_int(i64 0)
%args47412$k40461$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48673 = alloca %struct.ScmObj*, align 8
%args47412$k40461$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40462, %struct.ScmObj* %args47412$k40461$0)
store volatile %struct.ScmObj* %args47412$k40461$1, %struct.ScmObj** %stackaddr$prim48673, align 8
%stackaddr$prim48674 = alloca %struct.ScmObj*, align 8
%args47412$k40461$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43920, %struct.ScmObj* %args47412$k40461$1)
store volatile %struct.ScmObj* %args47412$k40461$2, %struct.ScmObj** %stackaddr$prim48674, align 8
%clofunc48675 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40461)
musttail call tailcc void %clofunc48675(%struct.ScmObj* %k40461, %struct.ScmObj* %args47412$k40461$2)
ret void
}

define tailcc void @proc_clo$ae43893(%struct.ScmObj* %env$ae43893,%struct.ScmObj* %current_45args47414) {
%stackaddr$prim48676 = alloca %struct.ScmObj*, align 8
%k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47414)
store volatile %struct.ScmObj* %k40463, %struct.ScmObj** %stackaddr$prim48676, align 8
%stackaddr$prim48677 = alloca %struct.ScmObj*, align 8
%current_45args47415 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47414)
store volatile %struct.ScmObj* %current_45args47415, %struct.ScmObj** %stackaddr$prim48677, align 8
%stackaddr$prim48678 = alloca %struct.ScmObj*, align 8
%x40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47415)
store volatile %struct.ScmObj* %x40165, %struct.ScmObj** %stackaddr$prim48678, align 8
%stackaddr$prim48679 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40165)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim48679, align 8
%stackaddr$prim48680 = alloca %struct.ScmObj*, align 8
%cpsprim40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40348)
store volatile %struct.ScmObj* %cpsprim40464, %struct.ScmObj** %stackaddr$prim48680, align 8
%ae43897 = call %struct.ScmObj* @const_init_int(i64 0)
%args47417$k40463$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48681 = alloca %struct.ScmObj*, align 8
%args47417$k40463$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40464, %struct.ScmObj* %args47417$k40463$0)
store volatile %struct.ScmObj* %args47417$k40463$1, %struct.ScmObj** %stackaddr$prim48681, align 8
%stackaddr$prim48682 = alloca %struct.ScmObj*, align 8
%args47417$k40463$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43897, %struct.ScmObj* %args47417$k40463$1)
store volatile %struct.ScmObj* %args47417$k40463$2, %struct.ScmObj** %stackaddr$prim48682, align 8
%clofunc48683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40463)
musttail call tailcc void %clofunc48683(%struct.ScmObj* %k40463, %struct.ScmObj* %args47417$k40463$2)
ret void
}

define tailcc void @proc_clo$ae43873(%struct.ScmObj* %env$ae43873,%struct.ScmObj* %current_45args47419) {
%stackaddr$prim48684 = alloca %struct.ScmObj*, align 8
%k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47419)
store volatile %struct.ScmObj* %k40465, %struct.ScmObj** %stackaddr$prim48684, align 8
%stackaddr$prim48685 = alloca %struct.ScmObj*, align 8
%current_45args47420 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47419)
store volatile %struct.ScmObj* %current_45args47420, %struct.ScmObj** %stackaddr$prim48685, align 8
%stackaddr$prim48686 = alloca %struct.ScmObj*, align 8
%x40167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47420)
store volatile %struct.ScmObj* %x40167, %struct.ScmObj** %stackaddr$prim48686, align 8
%stackaddr$prim48687 = alloca %struct.ScmObj*, align 8
%cpsprim40466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40167)
store volatile %struct.ScmObj* %cpsprim40466, %struct.ScmObj** %stackaddr$prim48687, align 8
%ae43876 = call %struct.ScmObj* @const_init_int(i64 0)
%args47422$k40465$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48688 = alloca %struct.ScmObj*, align 8
%args47422$k40465$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40466, %struct.ScmObj* %args47422$k40465$0)
store volatile %struct.ScmObj* %args47422$k40465$1, %struct.ScmObj** %stackaddr$prim48688, align 8
%stackaddr$prim48689 = alloca %struct.ScmObj*, align 8
%args47422$k40465$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43876, %struct.ScmObj* %args47422$k40465$1)
store volatile %struct.ScmObj* %args47422$k40465$2, %struct.ScmObj** %stackaddr$prim48689, align 8
%clofunc48690 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40465)
musttail call tailcc void %clofunc48690(%struct.ScmObj* %k40465, %struct.ScmObj* %args47422$k40465$2)
ret void
}

define tailcc void @proc_clo$ae43775(%struct.ScmObj* %env$ae43775,%struct.ScmObj* %args4016940467) {
%stackaddr$env-ref48691 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43775, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48691
%stackaddr$prim48692 = alloca %struct.ScmObj*, align 8
%k40468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4016940467)
store volatile %struct.ScmObj* %k40468, %struct.ScmObj** %stackaddr$prim48692, align 8
%stackaddr$prim48693 = alloca %struct.ScmObj*, align 8
%args40169 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4016940467)
store volatile %struct.ScmObj* %args40169, %struct.ScmObj** %stackaddr$prim48693, align 8
%stackaddr$prim48694 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim48694, align 8
%truthy$cmp48695 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40342)
%cmp$cmp48695 = icmp eq i64 %truthy$cmp48695, 1
br i1 %cmp$cmp48695, label %truebranch$cmp48695, label %falsebranch$cmp48695
truebranch$cmp48695:
%ae43781 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43782 = call %struct.ScmObj* @const_init_int(i64 1)
%args47424$k40468$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48696 = alloca %struct.ScmObj*, align 8
%args47424$k40468$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43782, %struct.ScmObj* %args47424$k40468$0)
store volatile %struct.ScmObj* %args47424$k40468$1, %struct.ScmObj** %stackaddr$prim48696, align 8
%stackaddr$prim48697 = alloca %struct.ScmObj*, align 8
%args47424$k40468$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43781, %struct.ScmObj* %args47424$k40468$1)
store volatile %struct.ScmObj* %args47424$k40468$2, %struct.ScmObj** %stackaddr$prim48697, align 8
%clofunc48698 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40468)
musttail call tailcc void %clofunc48698(%struct.ScmObj* %k40468, %struct.ScmObj* %args47424$k40468$2)
ret void
falsebranch$cmp48695:
%stackaddr$prim48699 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim48699, align 8
%stackaddr$prim48700 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40343)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim48700, align 8
%truthy$cmp48701 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40344)
%cmp$cmp48701 = icmp eq i64 %truthy$cmp48701, 1
br i1 %cmp$cmp48701, label %truebranch$cmp48701, label %falsebranch$cmp48701
truebranch$cmp48701:
%stackaddr$prim48702 = alloca %struct.ScmObj*, align 8
%cpsprim40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %cpsprim40469, %struct.ScmObj** %stackaddr$prim48702, align 8
%ae43794 = call %struct.ScmObj* @const_init_int(i64 0)
%args47425$k40468$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48703 = alloca %struct.ScmObj*, align 8
%args47425$k40468$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40469, %struct.ScmObj* %args47425$k40468$0)
store volatile %struct.ScmObj* %args47425$k40468$1, %struct.ScmObj** %stackaddr$prim48703, align 8
%stackaddr$prim48704 = alloca %struct.ScmObj*, align 8
%args47425$k40468$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43794, %struct.ScmObj* %args47425$k40468$1)
store volatile %struct.ScmObj* %args47425$k40468$2, %struct.ScmObj** %stackaddr$prim48704, align 8
%clofunc48705 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40468)
musttail call tailcc void %clofunc48705(%struct.ScmObj* %k40468, %struct.ScmObj* %args47425$k40468$2)
ret void
falsebranch$cmp48701:
%stackaddr$makeclosure48706 = alloca %struct.ScmObj*, align 8
%fptrToInt48707 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43799 to i64
%ae43799 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48707)
store volatile %struct.ScmObj* %ae43799, %struct.ScmObj** %stackaddr$makeclosure48706, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43799, %struct.ScmObj* %k40468, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43799, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43799, %struct.ScmObj* %args40169, i64 2)
%ae43800 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48708 = alloca %struct.ScmObj*, align 8
%fptrToInt48709 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43801 to i64
%ae43801 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48709)
store volatile %struct.ScmObj* %ae43801, %struct.ScmObj** %stackaddr$makeclosure48708, align 8
%args47435$ae43799$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48710 = alloca %struct.ScmObj*, align 8
%args47435$ae43799$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43801, %struct.ScmObj* %args47435$ae43799$0)
store volatile %struct.ScmObj* %args47435$ae43799$1, %struct.ScmObj** %stackaddr$prim48710, align 8
%stackaddr$prim48711 = alloca %struct.ScmObj*, align 8
%args47435$ae43799$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43800, %struct.ScmObj* %args47435$ae43799$1)
store volatile %struct.ScmObj* %args47435$ae43799$2, %struct.ScmObj** %stackaddr$prim48711, align 8
%clofunc48712 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43799)
musttail call tailcc void %clofunc48712(%struct.ScmObj* %ae43799, %struct.ScmObj* %args47435$ae43799$2)
ret void
}

define tailcc void @proc_clo$ae43799(%struct.ScmObj* %env$ae43799,%struct.ScmObj* %current_45args47426) {
%stackaddr$env-ref48713 = alloca %struct.ScmObj*, align 8
%k40468 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43799, i64 0)
store %struct.ScmObj* %k40468, %struct.ScmObj** %stackaddr$env-ref48713
%stackaddr$env-ref48714 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43799, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48714
%stackaddr$env-ref48715 = alloca %struct.ScmObj*, align 8
%args40169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43799, i64 2)
store %struct.ScmObj* %args40169, %struct.ScmObj** %stackaddr$env-ref48715
%stackaddr$prim48716 = alloca %struct.ScmObj*, align 8
%_95k40470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47426)
store volatile %struct.ScmObj* %_95k40470, %struct.ScmObj** %stackaddr$prim48716, align 8
%stackaddr$prim48717 = alloca %struct.ScmObj*, align 8
%current_45args47427 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47426)
store volatile %struct.ScmObj* %current_45args47427, %struct.ScmObj** %stackaddr$prim48717, align 8
%stackaddr$prim48718 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47427)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim48718, align 8
%stackaddr$prim48719 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim48719, align 8
%stackaddr$prim48720 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim48720, align 8
%args47429$_37foldl140108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48721 = alloca %struct.ScmObj*, align 8
%args47429$_37foldl140108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40347, %struct.ScmObj* %args47429$_37foldl140108$0)
store volatile %struct.ScmObj* %args47429$_37foldl140108$1, %struct.ScmObj** %stackaddr$prim48721, align 8
%stackaddr$prim48722 = alloca %struct.ScmObj*, align 8
%args47429$_37foldl140108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40346, %struct.ScmObj* %args47429$_37foldl140108$1)
store volatile %struct.ScmObj* %args47429$_37foldl140108$2, %struct.ScmObj** %stackaddr$prim48722, align 8
%stackaddr$prim48723 = alloca %struct.ScmObj*, align 8
%args47429$_37foldl140108$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40345, %struct.ScmObj* %args47429$_37foldl140108$2)
store volatile %struct.ScmObj* %args47429$_37foldl140108$3, %struct.ScmObj** %stackaddr$prim48723, align 8
%stackaddr$prim48724 = alloca %struct.ScmObj*, align 8
%args47429$_37foldl140108$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40468, %struct.ScmObj* %args47429$_37foldl140108$3)
store volatile %struct.ScmObj* %args47429$_37foldl140108$4, %struct.ScmObj** %stackaddr$prim48724, align 8
%clofunc48725 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140108)
musttail call tailcc void %clofunc48725(%struct.ScmObj* %_37foldl140108, %struct.ScmObj* %args47429$_37foldl140108$4)
ret void
}

define tailcc void @proc_clo$ae43801(%struct.ScmObj* %env$ae43801,%struct.ScmObj* %current_45args47430) {
%stackaddr$prim48726 = alloca %struct.ScmObj*, align 8
%k40471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47430)
store volatile %struct.ScmObj* %k40471, %struct.ScmObj** %stackaddr$prim48726, align 8
%stackaddr$prim48727 = alloca %struct.ScmObj*, align 8
%current_45args47431 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47430)
store volatile %struct.ScmObj* %current_45args47431, %struct.ScmObj** %stackaddr$prim48727, align 8
%stackaddr$prim48728 = alloca %struct.ScmObj*, align 8
%n40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47431)
store volatile %struct.ScmObj* %n40171, %struct.ScmObj** %stackaddr$prim48728, align 8
%stackaddr$prim48729 = alloca %struct.ScmObj*, align 8
%current_45args47432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47431)
store volatile %struct.ScmObj* %current_45args47432, %struct.ScmObj** %stackaddr$prim48729, align 8
%stackaddr$prim48730 = alloca %struct.ScmObj*, align 8
%v40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47432)
store volatile %struct.ScmObj* %v40170, %struct.ScmObj** %stackaddr$prim48730, align 8
%stackaddr$prim48731 = alloca %struct.ScmObj*, align 8
%cpsprim40472 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40170, %struct.ScmObj* %n40171)
store volatile %struct.ScmObj* %cpsprim40472, %struct.ScmObj** %stackaddr$prim48731, align 8
%ae43805 = call %struct.ScmObj* @const_init_int(i64 0)
%args47434$k40471$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48732 = alloca %struct.ScmObj*, align 8
%args47434$k40471$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40472, %struct.ScmObj* %args47434$k40471$0)
store volatile %struct.ScmObj* %args47434$k40471$1, %struct.ScmObj** %stackaddr$prim48732, align 8
%stackaddr$prim48733 = alloca %struct.ScmObj*, align 8
%args47434$k40471$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43805, %struct.ScmObj* %args47434$k40471$1)
store volatile %struct.ScmObj* %args47434$k40471$2, %struct.ScmObj** %stackaddr$prim48733, align 8
%clofunc48734 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40471)
musttail call tailcc void %clofunc48734(%struct.ScmObj* %k40471, %struct.ScmObj* %args47434$k40471$2)
ret void
}

define tailcc void @proc_clo$ae43371(%struct.ScmObj* %env$ae43371,%struct.ScmObj* %current_45args47437) {
%stackaddr$prim48735 = alloca %struct.ScmObj*, align 8
%k40473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47437)
store volatile %struct.ScmObj* %k40473, %struct.ScmObj** %stackaddr$prim48735, align 8
%stackaddr$prim48736 = alloca %struct.ScmObj*, align 8
%current_45args47438 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47437)
store volatile %struct.ScmObj* %current_45args47438, %struct.ScmObj** %stackaddr$prim48736, align 8
%stackaddr$prim48737 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47438)
store volatile %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$prim48737, align 8
%stackaddr$prim48738 = alloca %struct.ScmObj*, align 8
%current_45args47439 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47438)
store volatile %struct.ScmObj* %current_45args47439, %struct.ScmObj** %stackaddr$prim48738, align 8
%stackaddr$prim48739 = alloca %struct.ScmObj*, align 8
%lst40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47439)
store volatile %struct.ScmObj* %lst40173, %struct.ScmObj** %stackaddr$prim48739, align 8
%ae43372 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48740 = alloca %struct.ScmObj*, align 8
%lst40175 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43372, %struct.ScmObj* %lst40173)
store volatile %struct.ScmObj* %lst40175, %struct.ScmObj** %stackaddr$prim48740, align 8
%stackaddr$makeclosure48741 = alloca %struct.ScmObj*, align 8
%fptrToInt48742 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43374 to i64
%ae43374 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48742)
store volatile %struct.ScmObj* %ae43374, %struct.ScmObj** %stackaddr$makeclosure48741, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43374, %struct.ScmObj* %lst40175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43374, %struct.ScmObj* %v40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43374, %struct.ScmObj* %k40473, i64 2)
%ae43375 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48743 = alloca %struct.ScmObj*, align 8
%fptrToInt48744 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43376 to i64
%ae43376 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48744)
store volatile %struct.ScmObj* %ae43376, %struct.ScmObj** %stackaddr$makeclosure48743, align 8
%args47461$ae43374$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48745 = alloca %struct.ScmObj*, align 8
%args47461$ae43374$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43376, %struct.ScmObj* %args47461$ae43374$0)
store volatile %struct.ScmObj* %args47461$ae43374$1, %struct.ScmObj** %stackaddr$prim48745, align 8
%stackaddr$prim48746 = alloca %struct.ScmObj*, align 8
%args47461$ae43374$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43375, %struct.ScmObj* %args47461$ae43374$1)
store volatile %struct.ScmObj* %args47461$ae43374$2, %struct.ScmObj** %stackaddr$prim48746, align 8
%clofunc48747 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43374)
musttail call tailcc void %clofunc48747(%struct.ScmObj* %ae43374, %struct.ScmObj* %args47461$ae43374$2)
ret void
}

define tailcc void @proc_clo$ae43374(%struct.ScmObj* %env$ae43374,%struct.ScmObj* %current_45args47441) {
%stackaddr$env-ref48748 = alloca %struct.ScmObj*, align 8
%lst40175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43374, i64 0)
store %struct.ScmObj* %lst40175, %struct.ScmObj** %stackaddr$env-ref48748
%stackaddr$env-ref48749 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43374, i64 1)
store %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$env-ref48749
%stackaddr$env-ref48750 = alloca %struct.ScmObj*, align 8
%k40473 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43374, i64 2)
store %struct.ScmObj* %k40473, %struct.ScmObj** %stackaddr$env-ref48750
%stackaddr$prim48751 = alloca %struct.ScmObj*, align 8
%_95k40474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47441)
store volatile %struct.ScmObj* %_95k40474, %struct.ScmObj** %stackaddr$prim48751, align 8
%stackaddr$prim48752 = alloca %struct.ScmObj*, align 8
%current_45args47442 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47441)
store volatile %struct.ScmObj* %current_45args47442, %struct.ScmObj** %stackaddr$prim48752, align 8
%stackaddr$prim48753 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47442)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim48753, align 8
%stackaddr$makeclosure48754 = alloca %struct.ScmObj*, align 8
%fptrToInt48755 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43390 to i64
%ae43390 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48755)
store volatile %struct.ScmObj* %ae43390, %struct.ScmObj** %stackaddr$makeclosure48754, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43390, %struct.ScmObj* %lst40175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43390, %struct.ScmObj* %v40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43390, %struct.ScmObj* %k40473, i64 2)
%stackaddr$makeclosure48756 = alloca %struct.ScmObj*, align 8
%fptrToInt48757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43391 to i64
%ae43391 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48757)
store volatile %struct.ScmObj* %ae43391, %struct.ScmObj** %stackaddr$makeclosure48756, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43391, %struct.ScmObj* %lst40175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43391, %struct.ScmObj* %v40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43391, %struct.ScmObj* %k40473, i64 2)
%args47456$anf_45bind40334$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48758 = alloca %struct.ScmObj*, align 8
%args47456$anf_45bind40334$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43391, %struct.ScmObj* %args47456$anf_45bind40334$0)
store volatile %struct.ScmObj* %args47456$anf_45bind40334$1, %struct.ScmObj** %stackaddr$prim48758, align 8
%stackaddr$prim48759 = alloca %struct.ScmObj*, align 8
%args47456$anf_45bind40334$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43390, %struct.ScmObj* %args47456$anf_45bind40334$1)
store volatile %struct.ScmObj* %args47456$anf_45bind40334$2, %struct.ScmObj** %stackaddr$prim48759, align 8
%clofunc48760 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40334)
musttail call tailcc void %clofunc48760(%struct.ScmObj* %anf_45bind40334, %struct.ScmObj* %args47456$anf_45bind40334$2)
ret void
}

define tailcc void @proc_clo$ae43390(%struct.ScmObj* %env$ae43390,%struct.ScmObj* %current_45args47444) {
%stackaddr$env-ref48761 = alloca %struct.ScmObj*, align 8
%lst40175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43390, i64 0)
store %struct.ScmObj* %lst40175, %struct.ScmObj** %stackaddr$env-ref48761
%stackaddr$env-ref48762 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43390, i64 1)
store %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$env-ref48762
%stackaddr$env-ref48763 = alloca %struct.ScmObj*, align 8
%k40473 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43390, i64 2)
store %struct.ScmObj* %k40473, %struct.ScmObj** %stackaddr$env-ref48763
%stackaddr$prim48764 = alloca %struct.ScmObj*, align 8
%_95k40475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47444)
store volatile %struct.ScmObj* %_95k40475, %struct.ScmObj** %stackaddr$prim48764, align 8
%stackaddr$prim48765 = alloca %struct.ScmObj*, align 8
%current_45args47445 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47444)
store volatile %struct.ScmObj* %current_45args47445, %struct.ScmObj** %stackaddr$prim48765, align 8
%stackaddr$prim48766 = alloca %struct.ScmObj*, align 8
%cc40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47445)
store volatile %struct.ScmObj* %cc40176, %struct.ScmObj** %stackaddr$prim48766, align 8
%ae43499 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48767 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43499)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim48767, align 8
%stackaddr$prim48768 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40335)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim48768, align 8
%truthy$cmp48769 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40336)
%cmp$cmp48769 = icmp eq i64 %truthy$cmp48769, 1
br i1 %cmp$cmp48769, label %truebranch$cmp48769, label %falsebranch$cmp48769
truebranch$cmp48769:
%ae43503 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43504 = call %struct.ScmObj* @const_init_false()
%args47447$k40473$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48770 = alloca %struct.ScmObj*, align 8
%args47447$k40473$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43504, %struct.ScmObj* %args47447$k40473$0)
store volatile %struct.ScmObj* %args47447$k40473$1, %struct.ScmObj** %stackaddr$prim48770, align 8
%stackaddr$prim48771 = alloca %struct.ScmObj*, align 8
%args47447$k40473$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43503, %struct.ScmObj* %args47447$k40473$1)
store volatile %struct.ScmObj* %args47447$k40473$2, %struct.ScmObj** %stackaddr$prim48771, align 8
%clofunc48772 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40473)
musttail call tailcc void %clofunc48772(%struct.ScmObj* %k40473, %struct.ScmObj* %args47447$k40473$2)
ret void
falsebranch$cmp48769:
%ae43512 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48773 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43512)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim48773, align 8
%stackaddr$prim48774 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40337)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim48774, align 8
%stackaddr$prim48775 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40338, %struct.ScmObj* %v40174)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim48775, align 8
%truthy$cmp48776 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40339)
%cmp$cmp48776 = icmp eq i64 %truthy$cmp48776, 1
br i1 %cmp$cmp48776, label %truebranch$cmp48776, label %falsebranch$cmp48776
truebranch$cmp48776:
%ae43518 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48777 = alloca %struct.ScmObj*, align 8
%cpsprim40476 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43518)
store volatile %struct.ScmObj* %cpsprim40476, %struct.ScmObj** %stackaddr$prim48777, align 8
%ae43520 = call %struct.ScmObj* @const_init_int(i64 0)
%args47448$k40473$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48778 = alloca %struct.ScmObj*, align 8
%args47448$k40473$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40476, %struct.ScmObj* %args47448$k40473$0)
store volatile %struct.ScmObj* %args47448$k40473$1, %struct.ScmObj** %stackaddr$prim48778, align 8
%stackaddr$prim48779 = alloca %struct.ScmObj*, align 8
%args47448$k40473$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43520, %struct.ScmObj* %args47448$k40473$1)
store volatile %struct.ScmObj* %args47448$k40473$2, %struct.ScmObj** %stackaddr$prim48779, align 8
%clofunc48780 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40473)
musttail call tailcc void %clofunc48780(%struct.ScmObj* %k40473, %struct.ScmObj* %args47448$k40473$2)
ret void
falsebranch$cmp48776:
%ae43531 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48781 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43531)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim48781, align 8
%stackaddr$prim48782 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40340)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim48782, align 8
%ae43534 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48783 = alloca %struct.ScmObj*, align 8
%_95040178 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43534, %struct.ScmObj* %anf_45bind40341)
store volatile %struct.ScmObj* %_95040178, %struct.ScmObj** %stackaddr$prim48783, align 8
%args47449$cc40176$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48784 = alloca %struct.ScmObj*, align 8
%args47449$cc40176$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40176, %struct.ScmObj* %args47449$cc40176$0)
store volatile %struct.ScmObj* %args47449$cc40176$1, %struct.ScmObj** %stackaddr$prim48784, align 8
%stackaddr$prim48785 = alloca %struct.ScmObj*, align 8
%args47449$cc40176$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40473, %struct.ScmObj* %args47449$cc40176$1)
store volatile %struct.ScmObj* %args47449$cc40176$2, %struct.ScmObj** %stackaddr$prim48785, align 8
%clofunc48786 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40176)
musttail call tailcc void %clofunc48786(%struct.ScmObj* %cc40176, %struct.ScmObj* %args47449$cc40176$2)
ret void
}

define tailcc void @proc_clo$ae43391(%struct.ScmObj* %env$ae43391,%struct.ScmObj* %current_45args47450) {
%stackaddr$env-ref48787 = alloca %struct.ScmObj*, align 8
%lst40175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43391, i64 0)
store %struct.ScmObj* %lst40175, %struct.ScmObj** %stackaddr$env-ref48787
%stackaddr$env-ref48788 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43391, i64 1)
store %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$env-ref48788
%stackaddr$env-ref48789 = alloca %struct.ScmObj*, align 8
%k40473 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43391, i64 2)
store %struct.ScmObj* %k40473, %struct.ScmObj** %stackaddr$env-ref48789
%stackaddr$prim48790 = alloca %struct.ScmObj*, align 8
%_95k40475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47450)
store volatile %struct.ScmObj* %_95k40475, %struct.ScmObj** %stackaddr$prim48790, align 8
%stackaddr$prim48791 = alloca %struct.ScmObj*, align 8
%current_45args47451 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47450)
store volatile %struct.ScmObj* %current_45args47451, %struct.ScmObj** %stackaddr$prim48791, align 8
%stackaddr$prim48792 = alloca %struct.ScmObj*, align 8
%cc40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47451)
store volatile %struct.ScmObj* %cc40176, %struct.ScmObj** %stackaddr$prim48792, align 8
%ae43393 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48793 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43393)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim48793, align 8
%stackaddr$prim48794 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40335)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim48794, align 8
%truthy$cmp48795 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40336)
%cmp$cmp48795 = icmp eq i64 %truthy$cmp48795, 1
br i1 %cmp$cmp48795, label %truebranch$cmp48795, label %falsebranch$cmp48795
truebranch$cmp48795:
%ae43397 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43398 = call %struct.ScmObj* @const_init_false()
%args47453$k40473$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48796 = alloca %struct.ScmObj*, align 8
%args47453$k40473$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43398, %struct.ScmObj* %args47453$k40473$0)
store volatile %struct.ScmObj* %args47453$k40473$1, %struct.ScmObj** %stackaddr$prim48796, align 8
%stackaddr$prim48797 = alloca %struct.ScmObj*, align 8
%args47453$k40473$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43397, %struct.ScmObj* %args47453$k40473$1)
store volatile %struct.ScmObj* %args47453$k40473$2, %struct.ScmObj** %stackaddr$prim48797, align 8
%clofunc48798 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40473)
musttail call tailcc void %clofunc48798(%struct.ScmObj* %k40473, %struct.ScmObj* %args47453$k40473$2)
ret void
falsebranch$cmp48795:
%ae43406 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48799 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43406)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim48799, align 8
%stackaddr$prim48800 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40337)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim48800, align 8
%stackaddr$prim48801 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40338, %struct.ScmObj* %v40174)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim48801, align 8
%truthy$cmp48802 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40339)
%cmp$cmp48802 = icmp eq i64 %truthy$cmp48802, 1
br i1 %cmp$cmp48802, label %truebranch$cmp48802, label %falsebranch$cmp48802
truebranch$cmp48802:
%ae43412 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48803 = alloca %struct.ScmObj*, align 8
%cpsprim40476 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43412)
store volatile %struct.ScmObj* %cpsprim40476, %struct.ScmObj** %stackaddr$prim48803, align 8
%ae43414 = call %struct.ScmObj* @const_init_int(i64 0)
%args47454$k40473$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48804 = alloca %struct.ScmObj*, align 8
%args47454$k40473$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40476, %struct.ScmObj* %args47454$k40473$0)
store volatile %struct.ScmObj* %args47454$k40473$1, %struct.ScmObj** %stackaddr$prim48804, align 8
%stackaddr$prim48805 = alloca %struct.ScmObj*, align 8
%args47454$k40473$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43414, %struct.ScmObj* %args47454$k40473$1)
store volatile %struct.ScmObj* %args47454$k40473$2, %struct.ScmObj** %stackaddr$prim48805, align 8
%clofunc48806 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40473)
musttail call tailcc void %clofunc48806(%struct.ScmObj* %k40473, %struct.ScmObj* %args47454$k40473$2)
ret void
falsebranch$cmp48802:
%ae43425 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48807 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43425)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim48807, align 8
%stackaddr$prim48808 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40340)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim48808, align 8
%ae43428 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48809 = alloca %struct.ScmObj*, align 8
%_95040178 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43428, %struct.ScmObj* %anf_45bind40341)
store volatile %struct.ScmObj* %_95040178, %struct.ScmObj** %stackaddr$prim48809, align 8
%args47455$cc40176$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48810 = alloca %struct.ScmObj*, align 8
%args47455$cc40176$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40176, %struct.ScmObj* %args47455$cc40176$0)
store volatile %struct.ScmObj* %args47455$cc40176$1, %struct.ScmObj** %stackaddr$prim48810, align 8
%stackaddr$prim48811 = alloca %struct.ScmObj*, align 8
%args47455$cc40176$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40473, %struct.ScmObj* %args47455$cc40176$1)
store volatile %struct.ScmObj* %args47455$cc40176$2, %struct.ScmObj** %stackaddr$prim48811, align 8
%clofunc48812 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40176)
musttail call tailcc void %clofunc48812(%struct.ScmObj* %cc40176, %struct.ScmObj* %args47455$cc40176$2)
ret void
}

define tailcc void @proc_clo$ae43376(%struct.ScmObj* %env$ae43376,%struct.ScmObj* %current_45args47457) {
%stackaddr$prim48813 = alloca %struct.ScmObj*, align 8
%k40477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47457)
store volatile %struct.ScmObj* %k40477, %struct.ScmObj** %stackaddr$prim48813, align 8
%stackaddr$prim48814 = alloca %struct.ScmObj*, align 8
%current_45args47458 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47457)
store volatile %struct.ScmObj* %current_45args47458, %struct.ScmObj** %stackaddr$prim48814, align 8
%stackaddr$prim48815 = alloca %struct.ScmObj*, align 8
%u40177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47458)
store volatile %struct.ScmObj* %u40177, %struct.ScmObj** %stackaddr$prim48815, align 8
%args47460$u40177$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48816 = alloca %struct.ScmObj*, align 8
%args47460$u40177$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40177, %struct.ScmObj* %args47460$u40177$0)
store volatile %struct.ScmObj* %args47460$u40177$1, %struct.ScmObj** %stackaddr$prim48816, align 8
%stackaddr$prim48817 = alloca %struct.ScmObj*, align 8
%args47460$u40177$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40477, %struct.ScmObj* %args47460$u40177$1)
store volatile %struct.ScmObj* %args47460$u40177$2, %struct.ScmObj** %stackaddr$prim48817, align 8
%clofunc48818 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40177)
musttail call tailcc void %clofunc48818(%struct.ScmObj* %u40177, %struct.ScmObj* %args47460$u40177$2)
ret void
}

define tailcc void @proc_clo$ae42835(%struct.ScmObj* %env$ae42835,%struct.ScmObj* %current_45args47463) {
%stackaddr$prim48819 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47463)
store volatile %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$prim48819, align 8
%stackaddr$prim48820 = alloca %struct.ScmObj*, align 8
%current_45args47464 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47463)
store volatile %struct.ScmObj* %current_45args47464, %struct.ScmObj** %stackaddr$prim48820, align 8
%stackaddr$prim48821 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47464)
store volatile %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$prim48821, align 8
%stackaddr$prim48822 = alloca %struct.ScmObj*, align 8
%current_45args47465 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47464)
store volatile %struct.ScmObj* %current_45args47465, %struct.ScmObj** %stackaddr$prim48822, align 8
%stackaddr$prim48823 = alloca %struct.ScmObj*, align 8
%n40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47465)
store volatile %struct.ScmObj* %n40180, %struct.ScmObj** %stackaddr$prim48823, align 8
%ae42836 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48824 = alloca %struct.ScmObj*, align 8
%n40183 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42836, %struct.ScmObj* %n40180)
store volatile %struct.ScmObj* %n40183, %struct.ScmObj** %stackaddr$prim48824, align 8
%ae42838 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48825 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42838, %struct.ScmObj* %lst40181)
store volatile %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$prim48825, align 8
%stackaddr$makeclosure48826 = alloca %struct.ScmObj*, align 8
%fptrToInt48827 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42840 to i64
%ae42840 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48827)
store volatile %struct.ScmObj* %ae42840, %struct.ScmObj** %stackaddr$makeclosure48826, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42840, %struct.ScmObj* %n40183, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42840, %struct.ScmObj* %lst40182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42840, %struct.ScmObj* %k40478, i64 2)
%ae42841 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48828 = alloca %struct.ScmObj*, align 8
%fptrToInt48829 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42842 to i64
%ae42842 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48829)
store volatile %struct.ScmObj* %ae42842, %struct.ScmObj** %stackaddr$makeclosure48828, align 8
%args47485$ae42840$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48830 = alloca %struct.ScmObj*, align 8
%args47485$ae42840$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42842, %struct.ScmObj* %args47485$ae42840$0)
store volatile %struct.ScmObj* %args47485$ae42840$1, %struct.ScmObj** %stackaddr$prim48830, align 8
%stackaddr$prim48831 = alloca %struct.ScmObj*, align 8
%args47485$ae42840$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42841, %struct.ScmObj* %args47485$ae42840$1)
store volatile %struct.ScmObj* %args47485$ae42840$2, %struct.ScmObj** %stackaddr$prim48831, align 8
%clofunc48832 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42840)
musttail call tailcc void %clofunc48832(%struct.ScmObj* %ae42840, %struct.ScmObj* %args47485$ae42840$2)
ret void
}

define tailcc void @proc_clo$ae42840(%struct.ScmObj* %env$ae42840,%struct.ScmObj* %current_45args47467) {
%stackaddr$env-ref48833 = alloca %struct.ScmObj*, align 8
%n40183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42840, i64 0)
store %struct.ScmObj* %n40183, %struct.ScmObj** %stackaddr$env-ref48833
%stackaddr$env-ref48834 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42840, i64 1)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref48834
%stackaddr$env-ref48835 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42840, i64 2)
store %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$env-ref48835
%stackaddr$prim48836 = alloca %struct.ScmObj*, align 8
%_95k40479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47467)
store volatile %struct.ScmObj* %_95k40479, %struct.ScmObj** %stackaddr$prim48836, align 8
%stackaddr$prim48837 = alloca %struct.ScmObj*, align 8
%current_45args47468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47467)
store volatile %struct.ScmObj* %current_45args47468, %struct.ScmObj** %stackaddr$prim48837, align 8
%stackaddr$prim48838 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47468)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim48838, align 8
%stackaddr$makeclosure48839 = alloca %struct.ScmObj*, align 8
%fptrToInt48840 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42856 to i64
%ae42856 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48840)
store volatile %struct.ScmObj* %ae42856, %struct.ScmObj** %stackaddr$makeclosure48839, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42856, %struct.ScmObj* %n40183, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42856, %struct.ScmObj* %lst40182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42856, %struct.ScmObj* %k40478, i64 2)
%stackaddr$makeclosure48841 = alloca %struct.ScmObj*, align 8
%fptrToInt48842 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42857 to i64
%ae42857 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48842)
store volatile %struct.ScmObj* %ae42857, %struct.ScmObj** %stackaddr$makeclosure48841, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42857, %struct.ScmObj* %n40183, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42857, %struct.ScmObj* %lst40182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42857, %struct.ScmObj* %k40478, i64 2)
%args47480$anf_45bind40327$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48843 = alloca %struct.ScmObj*, align 8
%args47480$anf_45bind40327$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42857, %struct.ScmObj* %args47480$anf_45bind40327$0)
store volatile %struct.ScmObj* %args47480$anf_45bind40327$1, %struct.ScmObj** %stackaddr$prim48843, align 8
%stackaddr$prim48844 = alloca %struct.ScmObj*, align 8
%args47480$anf_45bind40327$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42856, %struct.ScmObj* %args47480$anf_45bind40327$1)
store volatile %struct.ScmObj* %args47480$anf_45bind40327$2, %struct.ScmObj** %stackaddr$prim48844, align 8
%clofunc48845 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40327)
musttail call tailcc void %clofunc48845(%struct.ScmObj* %anf_45bind40327, %struct.ScmObj* %args47480$anf_45bind40327$2)
ret void
}

define tailcc void @proc_clo$ae42856(%struct.ScmObj* %env$ae42856,%struct.ScmObj* %current_45args47470) {
%stackaddr$env-ref48846 = alloca %struct.ScmObj*, align 8
%n40183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42856, i64 0)
store %struct.ScmObj* %n40183, %struct.ScmObj** %stackaddr$env-ref48846
%stackaddr$env-ref48847 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42856, i64 1)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref48847
%stackaddr$env-ref48848 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42856, i64 2)
store %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$env-ref48848
%stackaddr$prim48849 = alloca %struct.ScmObj*, align 8
%_95k40480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47470)
store volatile %struct.ScmObj* %_95k40480, %struct.ScmObj** %stackaddr$prim48849, align 8
%stackaddr$prim48850 = alloca %struct.ScmObj*, align 8
%current_45args47471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47470)
store volatile %struct.ScmObj* %current_45args47471, %struct.ScmObj** %stackaddr$prim48850, align 8
%stackaddr$prim48851 = alloca %struct.ScmObj*, align 8
%cc40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47471)
store volatile %struct.ScmObj* %cc40184, %struct.ScmObj** %stackaddr$prim48851, align 8
%ae42999 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48852 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42999)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim48852, align 8
%ae43000 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48853 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae43000, %struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim48853, align 8
%truthy$cmp48854 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40329)
%cmp$cmp48854 = icmp eq i64 %truthy$cmp48854, 1
br i1 %cmp$cmp48854, label %truebranch$cmp48854, label %falsebranch$cmp48854
truebranch$cmp48854:
%ae43004 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48855 = alloca %struct.ScmObj*, align 8
%cpsprim40481 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43004)
store volatile %struct.ScmObj* %cpsprim40481, %struct.ScmObj** %stackaddr$prim48855, align 8
%ae43006 = call %struct.ScmObj* @const_init_int(i64 0)
%args47473$k40478$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48856 = alloca %struct.ScmObj*, align 8
%args47473$k40478$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40481, %struct.ScmObj* %args47473$k40478$0)
store volatile %struct.ScmObj* %args47473$k40478$1, %struct.ScmObj** %stackaddr$prim48856, align 8
%stackaddr$prim48857 = alloca %struct.ScmObj*, align 8
%args47473$k40478$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43006, %struct.ScmObj* %args47473$k40478$1)
store volatile %struct.ScmObj* %args47473$k40478$2, %struct.ScmObj** %stackaddr$prim48857, align 8
%clofunc48858 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40478)
musttail call tailcc void %clofunc48858(%struct.ScmObj* %k40478, %struct.ScmObj* %args47473$k40478$2)
ret void
falsebranch$cmp48854:
%ae43017 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48859 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43017)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim48859, align 8
%stackaddr$prim48860 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim48860, align 8
%ae43020 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48861 = alloca %struct.ScmObj*, align 8
%_95040187 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43020, %struct.ScmObj* %anf_45bind40331)
store volatile %struct.ScmObj* %_95040187, %struct.ScmObj** %stackaddr$prim48861, align 8
%ae43023 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48862 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40183, %struct.ScmObj* %ae43023)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim48862, align 8
%ae43025 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48863 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40332, %struct.ScmObj* %ae43025)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim48863, align 8
%ae43027 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48864 = alloca %struct.ScmObj*, align 8
%_95140186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40183, %struct.ScmObj* %ae43027, %struct.ScmObj* %anf_45bind40333)
store volatile %struct.ScmObj* %_95140186, %struct.ScmObj** %stackaddr$prim48864, align 8
%args47474$cc40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48865 = alloca %struct.ScmObj*, align 8
%args47474$cc40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40184, %struct.ScmObj* %args47474$cc40184$0)
store volatile %struct.ScmObj* %args47474$cc40184$1, %struct.ScmObj** %stackaddr$prim48865, align 8
%stackaddr$prim48866 = alloca %struct.ScmObj*, align 8
%args47474$cc40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40478, %struct.ScmObj* %args47474$cc40184$1)
store volatile %struct.ScmObj* %args47474$cc40184$2, %struct.ScmObj** %stackaddr$prim48866, align 8
%clofunc48867 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40184)
musttail call tailcc void %clofunc48867(%struct.ScmObj* %cc40184, %struct.ScmObj* %args47474$cc40184$2)
ret void
}

define tailcc void @proc_clo$ae42857(%struct.ScmObj* %env$ae42857,%struct.ScmObj* %current_45args47475) {
%stackaddr$env-ref48868 = alloca %struct.ScmObj*, align 8
%n40183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42857, i64 0)
store %struct.ScmObj* %n40183, %struct.ScmObj** %stackaddr$env-ref48868
%stackaddr$env-ref48869 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42857, i64 1)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref48869
%stackaddr$env-ref48870 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42857, i64 2)
store %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$env-ref48870
%stackaddr$prim48871 = alloca %struct.ScmObj*, align 8
%_95k40480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47475)
store volatile %struct.ScmObj* %_95k40480, %struct.ScmObj** %stackaddr$prim48871, align 8
%stackaddr$prim48872 = alloca %struct.ScmObj*, align 8
%current_45args47476 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47475)
store volatile %struct.ScmObj* %current_45args47476, %struct.ScmObj** %stackaddr$prim48872, align 8
%stackaddr$prim48873 = alloca %struct.ScmObj*, align 8
%cc40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47476)
store volatile %struct.ScmObj* %cc40184, %struct.ScmObj** %stackaddr$prim48873, align 8
%ae42859 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48874 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42859)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim48874, align 8
%ae42860 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48875 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42860, %struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim48875, align 8
%truthy$cmp48876 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40329)
%cmp$cmp48876 = icmp eq i64 %truthy$cmp48876, 1
br i1 %cmp$cmp48876, label %truebranch$cmp48876, label %falsebranch$cmp48876
truebranch$cmp48876:
%ae42864 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48877 = alloca %struct.ScmObj*, align 8
%cpsprim40481 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42864)
store volatile %struct.ScmObj* %cpsprim40481, %struct.ScmObj** %stackaddr$prim48877, align 8
%ae42866 = call %struct.ScmObj* @const_init_int(i64 0)
%args47478$k40478$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48878 = alloca %struct.ScmObj*, align 8
%args47478$k40478$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40481, %struct.ScmObj* %args47478$k40478$0)
store volatile %struct.ScmObj* %args47478$k40478$1, %struct.ScmObj** %stackaddr$prim48878, align 8
%stackaddr$prim48879 = alloca %struct.ScmObj*, align 8
%args47478$k40478$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42866, %struct.ScmObj* %args47478$k40478$1)
store volatile %struct.ScmObj* %args47478$k40478$2, %struct.ScmObj** %stackaddr$prim48879, align 8
%clofunc48880 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40478)
musttail call tailcc void %clofunc48880(%struct.ScmObj* %k40478, %struct.ScmObj* %args47478$k40478$2)
ret void
falsebranch$cmp48876:
%ae42877 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48881 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42877)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim48881, align 8
%stackaddr$prim48882 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim48882, align 8
%ae42880 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48883 = alloca %struct.ScmObj*, align 8
%_95040187 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42880, %struct.ScmObj* %anf_45bind40331)
store volatile %struct.ScmObj* %_95040187, %struct.ScmObj** %stackaddr$prim48883, align 8
%ae42883 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48884 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42883)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim48884, align 8
%ae42885 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48885 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40332, %struct.ScmObj* %ae42885)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim48885, align 8
%ae42887 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48886 = alloca %struct.ScmObj*, align 8
%_95140186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42887, %struct.ScmObj* %anf_45bind40333)
store volatile %struct.ScmObj* %_95140186, %struct.ScmObj** %stackaddr$prim48886, align 8
%args47479$cc40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48887 = alloca %struct.ScmObj*, align 8
%args47479$cc40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40184, %struct.ScmObj* %args47479$cc40184$0)
store volatile %struct.ScmObj* %args47479$cc40184$1, %struct.ScmObj** %stackaddr$prim48887, align 8
%stackaddr$prim48888 = alloca %struct.ScmObj*, align 8
%args47479$cc40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40478, %struct.ScmObj* %args47479$cc40184$1)
store volatile %struct.ScmObj* %args47479$cc40184$2, %struct.ScmObj** %stackaddr$prim48888, align 8
%clofunc48889 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40184)
musttail call tailcc void %clofunc48889(%struct.ScmObj* %cc40184, %struct.ScmObj* %args47479$cc40184$2)
ret void
}

define tailcc void @proc_clo$ae42842(%struct.ScmObj* %env$ae42842,%struct.ScmObj* %current_45args47481) {
%stackaddr$prim48890 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47481)
store volatile %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$prim48890, align 8
%stackaddr$prim48891 = alloca %struct.ScmObj*, align 8
%current_45args47482 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47481)
store volatile %struct.ScmObj* %current_45args47482, %struct.ScmObj** %stackaddr$prim48891, align 8
%stackaddr$prim48892 = alloca %struct.ScmObj*, align 8
%u40185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47482)
store volatile %struct.ScmObj* %u40185, %struct.ScmObj** %stackaddr$prim48892, align 8
%args47484$u40185$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48893 = alloca %struct.ScmObj*, align 8
%args47484$u40185$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40185, %struct.ScmObj* %args47484$u40185$0)
store volatile %struct.ScmObj* %args47484$u40185$1, %struct.ScmObj** %stackaddr$prim48893, align 8
%stackaddr$prim48894 = alloca %struct.ScmObj*, align 8
%args47484$u40185$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40482, %struct.ScmObj* %args47484$u40185$1)
store volatile %struct.ScmObj* %args47484$u40185$2, %struct.ScmObj** %stackaddr$prim48894, align 8
%clofunc48895 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40185)
musttail call tailcc void %clofunc48895(%struct.ScmObj* %u40185, %struct.ScmObj* %args47484$u40185$2)
ret void
}

define tailcc void @proc_clo$ae42419(%struct.ScmObj* %env$ae42419,%struct.ScmObj* %current_45args47487) {
%stackaddr$prim48896 = alloca %struct.ScmObj*, align 8
%k40483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47487)
store volatile %struct.ScmObj* %k40483, %struct.ScmObj** %stackaddr$prim48896, align 8
%stackaddr$prim48897 = alloca %struct.ScmObj*, align 8
%current_45args47488 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47487)
store volatile %struct.ScmObj* %current_45args47488, %struct.ScmObj** %stackaddr$prim48897, align 8
%stackaddr$prim48898 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47488)
store volatile %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$prim48898, align 8
%ae42420 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48899 = alloca %struct.ScmObj*, align 8
%a40190 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42420, %struct.ScmObj* %a40189)
store volatile %struct.ScmObj* %a40190, %struct.ScmObj** %stackaddr$prim48899, align 8
%stackaddr$makeclosure48900 = alloca %struct.ScmObj*, align 8
%fptrToInt48901 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42422 to i64
%ae42422 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48901)
store volatile %struct.ScmObj* %ae42422, %struct.ScmObj** %stackaddr$makeclosure48900, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42422, %struct.ScmObj* %k40483, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42422, %struct.ScmObj* %a40190, i64 1)
%ae42423 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48902 = alloca %struct.ScmObj*, align 8
%fptrToInt48903 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42424 to i64
%ae42424 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48903)
store volatile %struct.ScmObj* %ae42424, %struct.ScmObj** %stackaddr$makeclosure48902, align 8
%args47510$ae42422$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48904 = alloca %struct.ScmObj*, align 8
%args47510$ae42422$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42424, %struct.ScmObj* %args47510$ae42422$0)
store volatile %struct.ScmObj* %args47510$ae42422$1, %struct.ScmObj** %stackaddr$prim48904, align 8
%stackaddr$prim48905 = alloca %struct.ScmObj*, align 8
%args47510$ae42422$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42423, %struct.ScmObj* %args47510$ae42422$1)
store volatile %struct.ScmObj* %args47510$ae42422$2, %struct.ScmObj** %stackaddr$prim48905, align 8
%clofunc48906 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42422)
musttail call tailcc void %clofunc48906(%struct.ScmObj* %ae42422, %struct.ScmObj* %args47510$ae42422$2)
ret void
}

define tailcc void @proc_clo$ae42422(%struct.ScmObj* %env$ae42422,%struct.ScmObj* %current_45args47490) {
%stackaddr$env-ref48907 = alloca %struct.ScmObj*, align 8
%k40483 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42422, i64 0)
store %struct.ScmObj* %k40483, %struct.ScmObj** %stackaddr$env-ref48907
%stackaddr$env-ref48908 = alloca %struct.ScmObj*, align 8
%a40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42422, i64 1)
store %struct.ScmObj* %a40190, %struct.ScmObj** %stackaddr$env-ref48908
%stackaddr$prim48909 = alloca %struct.ScmObj*, align 8
%_95k40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47490)
store volatile %struct.ScmObj* %_95k40484, %struct.ScmObj** %stackaddr$prim48909, align 8
%stackaddr$prim48910 = alloca %struct.ScmObj*, align 8
%current_45args47491 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47490)
store volatile %struct.ScmObj* %current_45args47491, %struct.ScmObj** %stackaddr$prim48910, align 8
%stackaddr$prim48911 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47491)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim48911, align 8
%stackaddr$makeclosure48912 = alloca %struct.ScmObj*, align 8
%fptrToInt48913 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42441 to i64
%ae42441 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48913)
store volatile %struct.ScmObj* %ae42441, %struct.ScmObj** %stackaddr$makeclosure48912, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42441, %struct.ScmObj* %k40483, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42441, %struct.ScmObj* %a40190, i64 1)
%stackaddr$makeclosure48914 = alloca %struct.ScmObj*, align 8
%fptrToInt48915 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42442 to i64
%ae42442 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48915)
store volatile %struct.ScmObj* %ae42442, %struct.ScmObj** %stackaddr$makeclosure48914, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42442, %struct.ScmObj* %k40483, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42442, %struct.ScmObj* %a40190, i64 1)
%args47505$anf_45bind40319$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48916 = alloca %struct.ScmObj*, align 8
%args47505$anf_45bind40319$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42442, %struct.ScmObj* %args47505$anf_45bind40319$0)
store volatile %struct.ScmObj* %args47505$anf_45bind40319$1, %struct.ScmObj** %stackaddr$prim48916, align 8
%stackaddr$prim48917 = alloca %struct.ScmObj*, align 8
%args47505$anf_45bind40319$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42441, %struct.ScmObj* %args47505$anf_45bind40319$1)
store volatile %struct.ScmObj* %args47505$anf_45bind40319$2, %struct.ScmObj** %stackaddr$prim48917, align 8
%clofunc48918 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40319)
musttail call tailcc void %clofunc48918(%struct.ScmObj* %anf_45bind40319, %struct.ScmObj* %args47505$anf_45bind40319$2)
ret void
}

define tailcc void @proc_clo$ae42441(%struct.ScmObj* %env$ae42441,%struct.ScmObj* %current_45args47493) {
%stackaddr$env-ref48919 = alloca %struct.ScmObj*, align 8
%k40483 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42441, i64 0)
store %struct.ScmObj* %k40483, %struct.ScmObj** %stackaddr$env-ref48919
%stackaddr$env-ref48920 = alloca %struct.ScmObj*, align 8
%a40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42441, i64 1)
store %struct.ScmObj* %a40190, %struct.ScmObj** %stackaddr$env-ref48920
%stackaddr$prim48921 = alloca %struct.ScmObj*, align 8
%_95k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47493)
store volatile %struct.ScmObj* %_95k40485, %struct.ScmObj** %stackaddr$prim48921, align 8
%stackaddr$prim48922 = alloca %struct.ScmObj*, align 8
%current_45args47494 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47493)
store volatile %struct.ScmObj* %current_45args47494, %struct.ScmObj** %stackaddr$prim48922, align 8
%stackaddr$prim48923 = alloca %struct.ScmObj*, align 8
%cc40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47494)
store volatile %struct.ScmObj* %cc40191, %struct.ScmObj** %stackaddr$prim48923, align 8
%ae42557 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48924 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42557)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim48924, align 8
%stackaddr$prim48925 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim48925, align 8
%truthy$cmp48926 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40321)
%cmp$cmp48926 = icmp eq i64 %truthy$cmp48926, 1
br i1 %cmp$cmp48926, label %truebranch$cmp48926, label %falsebranch$cmp48926
truebranch$cmp48926:
%ae42561 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42562 = call %struct.ScmObj* @const_init_true()
%args47496$k40483$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48927 = alloca %struct.ScmObj*, align 8
%args47496$k40483$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42562, %struct.ScmObj* %args47496$k40483$0)
store volatile %struct.ScmObj* %args47496$k40483$1, %struct.ScmObj** %stackaddr$prim48927, align 8
%stackaddr$prim48928 = alloca %struct.ScmObj*, align 8
%args47496$k40483$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42561, %struct.ScmObj* %args47496$k40483$1)
store volatile %struct.ScmObj* %args47496$k40483$2, %struct.ScmObj** %stackaddr$prim48928, align 8
%clofunc48929 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40483)
musttail call tailcc void %clofunc48929(%struct.ScmObj* %k40483, %struct.ScmObj* %args47496$k40483$2)
ret void
falsebranch$cmp48926:
%ae42570 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48930 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42570)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim48930, align 8
%stackaddr$prim48931 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40322)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim48931, align 8
%truthy$cmp48932 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40323)
%cmp$cmp48932 = icmp eq i64 %truthy$cmp48932, 1
br i1 %cmp$cmp48932, label %truebranch$cmp48932, label %falsebranch$cmp48932
truebranch$cmp48932:
%ae42574 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48933 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42574)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim48933, align 8
%stackaddr$prim48934 = alloca %struct.ScmObj*, align 8
%b40193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %b40193, %struct.ScmObj** %stackaddr$prim48934, align 8
%ae42577 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48935 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42577)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim48935, align 8
%stackaddr$prim48936 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim48936, align 8
%ae42580 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48937 = alloca %struct.ScmObj*, align 8
%_95040194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42580, %struct.ScmObj* %anf_45bind40326)
store volatile %struct.ScmObj* %_95040194, %struct.ScmObj** %stackaddr$prim48937, align 8
%args47497$cc40191$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48938 = alloca %struct.ScmObj*, align 8
%args47497$cc40191$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40191, %struct.ScmObj* %args47497$cc40191$0)
store volatile %struct.ScmObj* %args47497$cc40191$1, %struct.ScmObj** %stackaddr$prim48938, align 8
%stackaddr$prim48939 = alloca %struct.ScmObj*, align 8
%args47497$cc40191$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40483, %struct.ScmObj* %args47497$cc40191$1)
store volatile %struct.ScmObj* %args47497$cc40191$2, %struct.ScmObj** %stackaddr$prim48939, align 8
%clofunc48940 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40191)
musttail call tailcc void %clofunc48940(%struct.ScmObj* %cc40191, %struct.ScmObj* %args47497$cc40191$2)
ret void
falsebranch$cmp48932:
%ae42613 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42614 = call %struct.ScmObj* @const_init_false()
%args47498$k40483$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48941 = alloca %struct.ScmObj*, align 8
%args47498$k40483$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42614, %struct.ScmObj* %args47498$k40483$0)
store volatile %struct.ScmObj* %args47498$k40483$1, %struct.ScmObj** %stackaddr$prim48941, align 8
%stackaddr$prim48942 = alloca %struct.ScmObj*, align 8
%args47498$k40483$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42613, %struct.ScmObj* %args47498$k40483$1)
store volatile %struct.ScmObj* %args47498$k40483$2, %struct.ScmObj** %stackaddr$prim48942, align 8
%clofunc48943 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40483)
musttail call tailcc void %clofunc48943(%struct.ScmObj* %k40483, %struct.ScmObj* %args47498$k40483$2)
ret void
}

define tailcc void @proc_clo$ae42442(%struct.ScmObj* %env$ae42442,%struct.ScmObj* %current_45args47499) {
%stackaddr$env-ref48944 = alloca %struct.ScmObj*, align 8
%k40483 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42442, i64 0)
store %struct.ScmObj* %k40483, %struct.ScmObj** %stackaddr$env-ref48944
%stackaddr$env-ref48945 = alloca %struct.ScmObj*, align 8
%a40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42442, i64 1)
store %struct.ScmObj* %a40190, %struct.ScmObj** %stackaddr$env-ref48945
%stackaddr$prim48946 = alloca %struct.ScmObj*, align 8
%_95k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47499)
store volatile %struct.ScmObj* %_95k40485, %struct.ScmObj** %stackaddr$prim48946, align 8
%stackaddr$prim48947 = alloca %struct.ScmObj*, align 8
%current_45args47500 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47499)
store volatile %struct.ScmObj* %current_45args47500, %struct.ScmObj** %stackaddr$prim48947, align 8
%stackaddr$prim48948 = alloca %struct.ScmObj*, align 8
%cc40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47500)
store volatile %struct.ScmObj* %cc40191, %struct.ScmObj** %stackaddr$prim48948, align 8
%ae42444 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48949 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42444)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim48949, align 8
%stackaddr$prim48950 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim48950, align 8
%truthy$cmp48951 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40321)
%cmp$cmp48951 = icmp eq i64 %truthy$cmp48951, 1
br i1 %cmp$cmp48951, label %truebranch$cmp48951, label %falsebranch$cmp48951
truebranch$cmp48951:
%ae42448 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42449 = call %struct.ScmObj* @const_init_true()
%args47502$k40483$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48952 = alloca %struct.ScmObj*, align 8
%args47502$k40483$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42449, %struct.ScmObj* %args47502$k40483$0)
store volatile %struct.ScmObj* %args47502$k40483$1, %struct.ScmObj** %stackaddr$prim48952, align 8
%stackaddr$prim48953 = alloca %struct.ScmObj*, align 8
%args47502$k40483$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42448, %struct.ScmObj* %args47502$k40483$1)
store volatile %struct.ScmObj* %args47502$k40483$2, %struct.ScmObj** %stackaddr$prim48953, align 8
%clofunc48954 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40483)
musttail call tailcc void %clofunc48954(%struct.ScmObj* %k40483, %struct.ScmObj* %args47502$k40483$2)
ret void
falsebranch$cmp48951:
%ae42457 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48955 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42457)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim48955, align 8
%stackaddr$prim48956 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40322)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim48956, align 8
%truthy$cmp48957 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40323)
%cmp$cmp48957 = icmp eq i64 %truthy$cmp48957, 1
br i1 %cmp$cmp48957, label %truebranch$cmp48957, label %falsebranch$cmp48957
truebranch$cmp48957:
%ae42461 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48958 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42461)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim48958, align 8
%stackaddr$prim48959 = alloca %struct.ScmObj*, align 8
%b40193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %b40193, %struct.ScmObj** %stackaddr$prim48959, align 8
%ae42464 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48960 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42464)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim48960, align 8
%stackaddr$prim48961 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim48961, align 8
%ae42467 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48962 = alloca %struct.ScmObj*, align 8
%_95040194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42467, %struct.ScmObj* %anf_45bind40326)
store volatile %struct.ScmObj* %_95040194, %struct.ScmObj** %stackaddr$prim48962, align 8
%args47503$cc40191$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48963 = alloca %struct.ScmObj*, align 8
%args47503$cc40191$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40191, %struct.ScmObj* %args47503$cc40191$0)
store volatile %struct.ScmObj* %args47503$cc40191$1, %struct.ScmObj** %stackaddr$prim48963, align 8
%stackaddr$prim48964 = alloca %struct.ScmObj*, align 8
%args47503$cc40191$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40483, %struct.ScmObj* %args47503$cc40191$1)
store volatile %struct.ScmObj* %args47503$cc40191$2, %struct.ScmObj** %stackaddr$prim48964, align 8
%clofunc48965 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40191)
musttail call tailcc void %clofunc48965(%struct.ScmObj* %cc40191, %struct.ScmObj* %args47503$cc40191$2)
ret void
falsebranch$cmp48957:
%ae42500 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42501 = call %struct.ScmObj* @const_init_false()
%args47504$k40483$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48966 = alloca %struct.ScmObj*, align 8
%args47504$k40483$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42501, %struct.ScmObj* %args47504$k40483$0)
store volatile %struct.ScmObj* %args47504$k40483$1, %struct.ScmObj** %stackaddr$prim48966, align 8
%stackaddr$prim48967 = alloca %struct.ScmObj*, align 8
%args47504$k40483$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42500, %struct.ScmObj* %args47504$k40483$1)
store volatile %struct.ScmObj* %args47504$k40483$2, %struct.ScmObj** %stackaddr$prim48967, align 8
%clofunc48968 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40483)
musttail call tailcc void %clofunc48968(%struct.ScmObj* %k40483, %struct.ScmObj* %args47504$k40483$2)
ret void
}

define tailcc void @proc_clo$ae42424(%struct.ScmObj* %env$ae42424,%struct.ScmObj* %current_45args47506) {
%stackaddr$prim48969 = alloca %struct.ScmObj*, align 8
%k40486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47506)
store volatile %struct.ScmObj* %k40486, %struct.ScmObj** %stackaddr$prim48969, align 8
%stackaddr$prim48970 = alloca %struct.ScmObj*, align 8
%current_45args47507 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47506)
store volatile %struct.ScmObj* %current_45args47507, %struct.ScmObj** %stackaddr$prim48970, align 8
%stackaddr$prim48971 = alloca %struct.ScmObj*, align 8
%k40192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47507)
store volatile %struct.ScmObj* %k40192, %struct.ScmObj** %stackaddr$prim48971, align 8
%ae42426 = call %struct.ScmObj* @const_init_int(i64 0)
%args47509$k40486$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48972 = alloca %struct.ScmObj*, align 8
%args47509$k40486$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40192, %struct.ScmObj* %args47509$k40486$0)
store volatile %struct.ScmObj* %args47509$k40486$1, %struct.ScmObj** %stackaddr$prim48972, align 8
%stackaddr$prim48973 = alloca %struct.ScmObj*, align 8
%args47509$k40486$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42426, %struct.ScmObj* %args47509$k40486$1)
store volatile %struct.ScmObj* %args47509$k40486$2, %struct.ScmObj** %stackaddr$prim48973, align 8
%clofunc48974 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40486)
musttail call tailcc void %clofunc48974(%struct.ScmObj* %k40486, %struct.ScmObj* %args47509$k40486$2)
ret void
}

define tailcc void @proc_clo$ae42347(%struct.ScmObj* %env$ae42347,%struct.ScmObj* %current_45args47512) {
%stackaddr$env-ref48975 = alloca %struct.ScmObj*, align 8
%_37append40196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42347, i64 0)
store %struct.ScmObj* %_37append40196, %struct.ScmObj** %stackaddr$env-ref48975
%stackaddr$prim48976 = alloca %struct.ScmObj*, align 8
%k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47512)
store volatile %struct.ScmObj* %k40487, %struct.ScmObj** %stackaddr$prim48976, align 8
%stackaddr$prim48977 = alloca %struct.ScmObj*, align 8
%current_45args47513 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47512)
store volatile %struct.ScmObj* %current_45args47513, %struct.ScmObj** %stackaddr$prim48977, align 8
%stackaddr$prim48978 = alloca %struct.ScmObj*, align 8
%ls040199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47513)
store volatile %struct.ScmObj* %ls040199, %struct.ScmObj** %stackaddr$prim48978, align 8
%stackaddr$prim48979 = alloca %struct.ScmObj*, align 8
%current_45args47514 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47513)
store volatile %struct.ScmObj* %current_45args47514, %struct.ScmObj** %stackaddr$prim48979, align 8
%stackaddr$prim48980 = alloca %struct.ScmObj*, align 8
%ls140198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47514)
store volatile %struct.ScmObj* %ls140198, %struct.ScmObj** %stackaddr$prim48980, align 8
%stackaddr$prim48981 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040199)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim48981, align 8
%truthy$cmp48982 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40313)
%cmp$cmp48982 = icmp eq i64 %truthy$cmp48982, 1
br i1 %cmp$cmp48982, label %truebranch$cmp48982, label %falsebranch$cmp48982
truebranch$cmp48982:
%ae42351 = call %struct.ScmObj* @const_init_int(i64 0)
%args47516$k40487$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48983 = alloca %struct.ScmObj*, align 8
%args47516$k40487$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140198, %struct.ScmObj* %args47516$k40487$0)
store volatile %struct.ScmObj* %args47516$k40487$1, %struct.ScmObj** %stackaddr$prim48983, align 8
%stackaddr$prim48984 = alloca %struct.ScmObj*, align 8
%args47516$k40487$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42351, %struct.ScmObj* %args47516$k40487$1)
store volatile %struct.ScmObj* %args47516$k40487$2, %struct.ScmObj** %stackaddr$prim48984, align 8
%clofunc48985 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40487)
musttail call tailcc void %clofunc48985(%struct.ScmObj* %k40487, %struct.ScmObj* %args47516$k40487$2)
ret void
falsebranch$cmp48982:
%stackaddr$prim48986 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040199)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim48986, align 8
%ae42358 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48987 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40196, %struct.ScmObj* %ae42358)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim48987, align 8
%stackaddr$prim48988 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040199)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim48988, align 8
%stackaddr$makeclosure48989 = alloca %struct.ScmObj*, align 8
%fptrToInt48990 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42361 to i64
%ae42361 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48990)
store volatile %struct.ScmObj* %ae42361, %struct.ScmObj** %stackaddr$makeclosure48989, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42361, %struct.ScmObj* %k40487, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42361, %struct.ScmObj* %anf_45bind40314, i64 1)
%args47521$anf_45bind40315$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48991 = alloca %struct.ScmObj*, align 8
%args47521$anf_45bind40315$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140198, %struct.ScmObj* %args47521$anf_45bind40315$0)
store volatile %struct.ScmObj* %args47521$anf_45bind40315$1, %struct.ScmObj** %stackaddr$prim48991, align 8
%stackaddr$prim48992 = alloca %struct.ScmObj*, align 8
%args47521$anf_45bind40315$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40316, %struct.ScmObj* %args47521$anf_45bind40315$1)
store volatile %struct.ScmObj* %args47521$anf_45bind40315$2, %struct.ScmObj** %stackaddr$prim48992, align 8
%stackaddr$prim48993 = alloca %struct.ScmObj*, align 8
%args47521$anf_45bind40315$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42361, %struct.ScmObj* %args47521$anf_45bind40315$2)
store volatile %struct.ScmObj* %args47521$anf_45bind40315$3, %struct.ScmObj** %stackaddr$prim48993, align 8
%clofunc48994 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40315)
musttail call tailcc void %clofunc48994(%struct.ScmObj* %anf_45bind40315, %struct.ScmObj* %args47521$anf_45bind40315$3)
ret void
}

define tailcc void @proc_clo$ae42361(%struct.ScmObj* %env$ae42361,%struct.ScmObj* %current_45args47517) {
%stackaddr$env-ref48995 = alloca %struct.ScmObj*, align 8
%k40487 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42361, i64 0)
store %struct.ScmObj* %k40487, %struct.ScmObj** %stackaddr$env-ref48995
%stackaddr$env-ref48996 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42361, i64 1)
store %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$env-ref48996
%stackaddr$prim48997 = alloca %struct.ScmObj*, align 8
%_95k40488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47517)
store volatile %struct.ScmObj* %_95k40488, %struct.ScmObj** %stackaddr$prim48997, align 8
%stackaddr$prim48998 = alloca %struct.ScmObj*, align 8
%current_45args47518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47517)
store volatile %struct.ScmObj* %current_45args47518, %struct.ScmObj** %stackaddr$prim48998, align 8
%stackaddr$prim48999 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47518)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim48999, align 8
%stackaddr$prim49000 = alloca %struct.ScmObj*, align 8
%cpsprim40489 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40314, %struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %cpsprim40489, %struct.ScmObj** %stackaddr$prim49000, align 8
%ae42367 = call %struct.ScmObj* @const_init_int(i64 0)
%args47520$k40487$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49001 = alloca %struct.ScmObj*, align 8
%args47520$k40487$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40489, %struct.ScmObj* %args47520$k40487$0)
store volatile %struct.ScmObj* %args47520$k40487$1, %struct.ScmObj** %stackaddr$prim49001, align 8
%stackaddr$prim49002 = alloca %struct.ScmObj*, align 8
%args47520$k40487$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42367, %struct.ScmObj* %args47520$k40487$1)
store volatile %struct.ScmObj* %args47520$k40487$2, %struct.ScmObj** %stackaddr$prim49002, align 8
%clofunc49003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40487)
musttail call tailcc void %clofunc49003(%struct.ScmObj* %k40487, %struct.ScmObj* %args47520$k40487$2)
ret void
}

define tailcc void @proc_clo$ae42321(%struct.ScmObj* %env$ae42321,%struct.ScmObj* %current_45args47523) {
%stackaddr$prim49004 = alloca %struct.ScmObj*, align 8
%k40490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47523)
store volatile %struct.ScmObj* %k40490, %struct.ScmObj** %stackaddr$prim49004, align 8
%stackaddr$prim49005 = alloca %struct.ScmObj*, align 8
%current_45args47524 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47523)
store volatile %struct.ScmObj* %current_45args47524, %struct.ScmObj** %stackaddr$prim49005, align 8
%stackaddr$prim49006 = alloca %struct.ScmObj*, align 8
%a40202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47524)
store volatile %struct.ScmObj* %a40202, %struct.ScmObj** %stackaddr$prim49006, align 8
%stackaddr$prim49007 = alloca %struct.ScmObj*, align 8
%current_45args47525 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47524)
store volatile %struct.ScmObj* %current_45args47525, %struct.ScmObj** %stackaddr$prim49007, align 8
%stackaddr$prim49008 = alloca %struct.ScmObj*, align 8
%b40201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47525)
store volatile %struct.ScmObj* %b40201, %struct.ScmObj** %stackaddr$prim49008, align 8
%stackaddr$prim49009 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40202, %struct.ScmObj* %b40201)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim49009, align 8
%stackaddr$prim49010 = alloca %struct.ScmObj*, align 8
%cpsprim40491 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %cpsprim40491, %struct.ScmObj** %stackaddr$prim49010, align 8
%ae42326 = call %struct.ScmObj* @const_init_int(i64 0)
%args47527$k40490$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49011 = alloca %struct.ScmObj*, align 8
%args47527$k40490$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40491, %struct.ScmObj* %args47527$k40490$0)
store volatile %struct.ScmObj* %args47527$k40490$1, %struct.ScmObj** %stackaddr$prim49011, align 8
%stackaddr$prim49012 = alloca %struct.ScmObj*, align 8
%args47527$k40490$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42326, %struct.ScmObj* %args47527$k40490$1)
store volatile %struct.ScmObj* %args47527$k40490$2, %struct.ScmObj** %stackaddr$prim49012, align 8
%clofunc49013 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40490)
musttail call tailcc void %clofunc49013(%struct.ScmObj* %k40490, %struct.ScmObj* %args47527$k40490$2)
ret void
}

define tailcc void @proc_clo$ae42297(%struct.ScmObj* %env$ae42297,%struct.ScmObj* %current_45args47529) {
%stackaddr$prim49014 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47529)
store volatile %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$prim49014, align 8
%stackaddr$prim49015 = alloca %struct.ScmObj*, align 8
%current_45args47530 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47529)
store volatile %struct.ScmObj* %current_45args47530, %struct.ScmObj** %stackaddr$prim49015, align 8
%stackaddr$prim49016 = alloca %struct.ScmObj*, align 8
%a40205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47530)
store volatile %struct.ScmObj* %a40205, %struct.ScmObj** %stackaddr$prim49016, align 8
%stackaddr$prim49017 = alloca %struct.ScmObj*, align 8
%current_45args47531 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47530)
store volatile %struct.ScmObj* %current_45args47531, %struct.ScmObj** %stackaddr$prim49017, align 8
%stackaddr$prim49018 = alloca %struct.ScmObj*, align 8
%b40204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47531)
store volatile %struct.ScmObj* %b40204, %struct.ScmObj** %stackaddr$prim49018, align 8
%stackaddr$prim49019 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40205, %struct.ScmObj* %b40204)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim49019, align 8
%stackaddr$prim49020 = alloca %struct.ScmObj*, align 8
%cpsprim40493 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %cpsprim40493, %struct.ScmObj** %stackaddr$prim49020, align 8
%ae42302 = call %struct.ScmObj* @const_init_int(i64 0)
%args47533$k40492$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49021 = alloca %struct.ScmObj*, align 8
%args47533$k40492$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40493, %struct.ScmObj* %args47533$k40492$0)
store volatile %struct.ScmObj* %args47533$k40492$1, %struct.ScmObj** %stackaddr$prim49021, align 8
%stackaddr$prim49022 = alloca %struct.ScmObj*, align 8
%args47533$k40492$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42302, %struct.ScmObj* %args47533$k40492$1)
store volatile %struct.ScmObj* %args47533$k40492$2, %struct.ScmObj** %stackaddr$prim49022, align 8
%clofunc49023 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40492)
musttail call tailcc void %clofunc49023(%struct.ScmObj* %k40492, %struct.ScmObj* %args47533$k40492$2)
ret void
}

define tailcc void @proc_clo$ae41903(%struct.ScmObj* %env$ae41903,%struct.ScmObj* %current_45args47536) {
%stackaddr$env-ref49024 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 0)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49024
%stackaddr$env-ref49025 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49025
%stackaddr$env-ref49026 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 2)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref49026
%stackaddr$prim49027 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47536)
store volatile %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$prim49027, align 8
%stackaddr$prim49028 = alloca %struct.ScmObj*, align 8
%current_45args47537 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47536)
store volatile %struct.ScmObj* %current_45args47537, %struct.ScmObj** %stackaddr$prim49028, align 8
%stackaddr$prim49029 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47537)
store volatile %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$prim49029, align 8
%ae41905 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49030 = alloca %struct.ScmObj*, align 8
%fptrToInt49031 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41906 to i64
%ae41906 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49031)
store volatile %struct.ScmObj* %ae41906, %struct.ScmObj** %stackaddr$makeclosure49030, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %_37foldr40129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %_37foldl40207, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %_37foldr140124, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %_37map140155, i64 3)
%args47594$k40494$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49032 = alloca %struct.ScmObj*, align 8
%args47594$k40494$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41906, %struct.ScmObj* %args47594$k40494$0)
store volatile %struct.ScmObj* %args47594$k40494$1, %struct.ScmObj** %stackaddr$prim49032, align 8
%stackaddr$prim49033 = alloca %struct.ScmObj*, align 8
%args47594$k40494$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41905, %struct.ScmObj* %args47594$k40494$1)
store volatile %struct.ScmObj* %args47594$k40494$2, %struct.ScmObj** %stackaddr$prim49033, align 8
%clofunc49034 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40494)
musttail call tailcc void %clofunc49034(%struct.ScmObj* %k40494, %struct.ScmObj* %args47594$k40494$2)
ret void
}

define tailcc void @proc_clo$ae41906(%struct.ScmObj* %env$ae41906,%struct.ScmObj* %args4020840495) {
%stackaddr$env-ref49035 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 0)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49035
%stackaddr$env-ref49036 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 1)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref49036
%stackaddr$env-ref49037 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49037
%stackaddr$env-ref49038 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 3)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref49038
%stackaddr$prim49039 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4020840495)
store volatile %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$prim49039, align 8
%stackaddr$prim49040 = alloca %struct.ScmObj*, align 8
%args40208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4020840495)
store volatile %struct.ScmObj* %args40208, %struct.ScmObj** %stackaddr$prim49040, align 8
%stackaddr$prim49041 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40208)
store volatile %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$prim49041, align 8
%stackaddr$prim49042 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40208)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim49042, align 8
%stackaddr$prim49043 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40299)
store volatile %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$prim49043, align 8
%stackaddr$prim49044 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40208)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim49044, align 8
%stackaddr$prim49045 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40300)
store volatile %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$prim49045, align 8
%stackaddr$makeclosure49046 = alloca %struct.ScmObj*, align 8
%fptrToInt49047 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41914 to i64
%ae41914 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt49047)
store volatile %struct.ScmObj* %ae41914, %struct.ScmObj** %stackaddr$makeclosure49046, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41914, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41914, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41914, %struct.ScmObj* %f40211, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41914, %struct.ScmObj* %acc40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41914, %struct.ScmObj* %k40496, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41914, %struct.ScmObj* %_37foldl40207, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41914, %struct.ScmObj* %_37foldr140124, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41914, %struct.ScmObj* %_37map140155, i64 7)
%ae41915 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49048 = alloca %struct.ScmObj*, align 8
%fptrToInt49049 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41916 to i64
%ae41916 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49049)
store volatile %struct.ScmObj* %ae41916, %struct.ScmObj** %stackaddr$makeclosure49048, align 8
%args47593$ae41914$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49050 = alloca %struct.ScmObj*, align 8
%args47593$ae41914$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41916, %struct.ScmObj* %args47593$ae41914$0)
store volatile %struct.ScmObj* %args47593$ae41914$1, %struct.ScmObj** %stackaddr$prim49050, align 8
%stackaddr$prim49051 = alloca %struct.ScmObj*, align 8
%args47593$ae41914$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41915, %struct.ScmObj* %args47593$ae41914$1)
store volatile %struct.ScmObj* %args47593$ae41914$2, %struct.ScmObj** %stackaddr$prim49051, align 8
%clofunc49052 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41914)
musttail call tailcc void %clofunc49052(%struct.ScmObj* %ae41914, %struct.ScmObj* %args47593$ae41914$2)
ret void
}

define tailcc void @proc_clo$ae41914(%struct.ScmObj* %env$ae41914,%struct.ScmObj* %current_45args47539) {
%stackaddr$env-ref49053 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41914, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref49053
%stackaddr$env-ref49054 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41914, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49054
%stackaddr$env-ref49055 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41914, i64 2)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref49055
%stackaddr$env-ref49056 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41914, i64 3)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref49056
%stackaddr$env-ref49057 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41914, i64 4)
store %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$env-ref49057
%stackaddr$env-ref49058 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41914, i64 5)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref49058
%stackaddr$env-ref49059 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41914, i64 6)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49059
%stackaddr$env-ref49060 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41914, i64 7)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref49060
%stackaddr$prim49061 = alloca %struct.ScmObj*, align 8
%_95k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47539)
store volatile %struct.ScmObj* %_95k40497, %struct.ScmObj** %stackaddr$prim49061, align 8
%stackaddr$prim49062 = alloca %struct.ScmObj*, align 8
%current_45args47540 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47539)
store volatile %struct.ScmObj* %current_45args47540, %struct.ScmObj** %stackaddr$prim49062, align 8
%stackaddr$prim49063 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47540)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim49063, align 8
%stackaddr$makeclosure49064 = alloca %struct.ScmObj*, align 8
%fptrToInt49065 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41946 to i64
%ae41946 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49065)
store volatile %struct.ScmObj* %ae41946, %struct.ScmObj** %stackaddr$makeclosure49064, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %f40211, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %acc40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %k40496, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %_37foldl40207, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %_37map140155, i64 6)
%ae41948 = call %struct.ScmObj* @const_init_false()
%args47586$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49066 = alloca %struct.ScmObj*, align 8
%args47586$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40209, %struct.ScmObj* %args47586$_37foldr140124$0)
store volatile %struct.ScmObj* %args47586$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim49066, align 8
%stackaddr$prim49067 = alloca %struct.ScmObj*, align 8
%args47586$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41948, %struct.ScmObj* %args47586$_37foldr140124$1)
store volatile %struct.ScmObj* %args47586$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim49067, align 8
%stackaddr$prim49068 = alloca %struct.ScmObj*, align 8
%args47586$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40301, %struct.ScmObj* %args47586$_37foldr140124$2)
store volatile %struct.ScmObj* %args47586$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim49068, align 8
%stackaddr$prim49069 = alloca %struct.ScmObj*, align 8
%args47586$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41946, %struct.ScmObj* %args47586$_37foldr140124$3)
store volatile %struct.ScmObj* %args47586$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim49069, align 8
%clofunc49070 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc49070(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args47586$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae41946(%struct.ScmObj* %env$ae41946,%struct.ScmObj* %current_45args47542) {
%stackaddr$env-ref49071 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref49071
%stackaddr$env-ref49072 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49072
%stackaddr$env-ref49073 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 2)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref49073
%stackaddr$env-ref49074 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 3)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref49074
%stackaddr$env-ref49075 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 4)
store %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$env-ref49075
%stackaddr$env-ref49076 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 5)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref49076
%stackaddr$env-ref49077 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 6)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref49077
%stackaddr$prim49078 = alloca %struct.ScmObj*, align 8
%_95k40498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47542)
store volatile %struct.ScmObj* %_95k40498, %struct.ScmObj** %stackaddr$prim49078, align 8
%stackaddr$prim49079 = alloca %struct.ScmObj*, align 8
%current_45args47543 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47542)
store volatile %struct.ScmObj* %current_45args47543, %struct.ScmObj** %stackaddr$prim49079, align 8
%stackaddr$prim49080 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47543)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim49080, align 8
%truthy$cmp49081 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40302)
%cmp$cmp49081 = icmp eq i64 %truthy$cmp49081, 1
br i1 %cmp$cmp49081, label %truebranch$cmp49081, label %falsebranch$cmp49081
truebranch$cmp49081:
%ae41957 = call %struct.ScmObj* @const_init_int(i64 0)
%args47545$k40496$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49082 = alloca %struct.ScmObj*, align 8
%args47545$k40496$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40210, %struct.ScmObj* %args47545$k40496$0)
store volatile %struct.ScmObj* %args47545$k40496$1, %struct.ScmObj** %stackaddr$prim49082, align 8
%stackaddr$prim49083 = alloca %struct.ScmObj*, align 8
%args47545$k40496$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41957, %struct.ScmObj* %args47545$k40496$1)
store volatile %struct.ScmObj* %args47545$k40496$2, %struct.ScmObj** %stackaddr$prim49083, align 8
%clofunc49084 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40496)
musttail call tailcc void %clofunc49084(%struct.ScmObj* %k40496, %struct.ScmObj* %args47545$k40496$2)
ret void
falsebranch$cmp49081:
%stackaddr$makeclosure49085 = alloca %struct.ScmObj*, align 8
%fptrToInt49086 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41962 to i64
%ae41962 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49086)
store volatile %struct.ScmObj* %ae41962, %struct.ScmObj** %stackaddr$makeclosure49085, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %f40211, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %acc40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %k40496, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %_37foldl40207, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %_37map140155, i64 6)
%ae41963 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49087 = alloca %struct.ScmObj*, align 8
%fptrToInt49088 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41964 to i64
%ae41964 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49088)
store volatile %struct.ScmObj* %ae41964, %struct.ScmObj** %stackaddr$makeclosure49087, align 8
%args47585$ae41962$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49089 = alloca %struct.ScmObj*, align 8
%args47585$ae41962$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41964, %struct.ScmObj* %args47585$ae41962$0)
store volatile %struct.ScmObj* %args47585$ae41962$1, %struct.ScmObj** %stackaddr$prim49089, align 8
%stackaddr$prim49090 = alloca %struct.ScmObj*, align 8
%args47585$ae41962$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41963, %struct.ScmObj* %args47585$ae41962$1)
store volatile %struct.ScmObj* %args47585$ae41962$2, %struct.ScmObj** %stackaddr$prim49090, align 8
%clofunc49091 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41962)
musttail call tailcc void %clofunc49091(%struct.ScmObj* %ae41962, %struct.ScmObj* %args47585$ae41962$2)
ret void
}

define tailcc void @proc_clo$ae41962(%struct.ScmObj* %env$ae41962,%struct.ScmObj* %current_45args47546) {
%stackaddr$env-ref49092 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref49092
%stackaddr$env-ref49093 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49093
%stackaddr$env-ref49094 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 2)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref49094
%stackaddr$env-ref49095 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 3)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref49095
%stackaddr$env-ref49096 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 4)
store %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$env-ref49096
%stackaddr$env-ref49097 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 5)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref49097
%stackaddr$env-ref49098 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 6)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref49098
%stackaddr$prim49099 = alloca %struct.ScmObj*, align 8
%_95k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47546)
store volatile %struct.ScmObj* %_95k40499, %struct.ScmObj** %stackaddr$prim49099, align 8
%stackaddr$prim49100 = alloca %struct.ScmObj*, align 8
%current_45args47547 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47546)
store volatile %struct.ScmObj* %current_45args47547, %struct.ScmObj** %stackaddr$prim49100, align 8
%stackaddr$prim49101 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47547)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim49101, align 8
%stackaddr$makeclosure49102 = alloca %struct.ScmObj*, align 8
%fptrToInt49103 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41983 to i64
%ae41983 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49103)
store volatile %struct.ScmObj* %ae41983, %struct.ScmObj** %stackaddr$makeclosure49102, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %f40211, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %acc40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %k40496, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %_37foldl40207, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %_37map140155, i64 6)
%args47580$_37map140155$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49104 = alloca %struct.ScmObj*, align 8
%args47580$_37map140155$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40209, %struct.ScmObj* %args47580$_37map140155$0)
store volatile %struct.ScmObj* %args47580$_37map140155$1, %struct.ScmObj** %stackaddr$prim49104, align 8
%stackaddr$prim49105 = alloca %struct.ScmObj*, align 8
%args47580$_37map140155$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40303, %struct.ScmObj* %args47580$_37map140155$1)
store volatile %struct.ScmObj* %args47580$_37map140155$2, %struct.ScmObj** %stackaddr$prim49105, align 8
%stackaddr$prim49106 = alloca %struct.ScmObj*, align 8
%args47580$_37map140155$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41983, %struct.ScmObj* %args47580$_37map140155$2)
store volatile %struct.ScmObj* %args47580$_37map140155$3, %struct.ScmObj** %stackaddr$prim49106, align 8
%clofunc49107 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140155)
musttail call tailcc void %clofunc49107(%struct.ScmObj* %_37map140155, %struct.ScmObj* %args47580$_37map140155$3)
ret void
}

define tailcc void @proc_clo$ae41983(%struct.ScmObj* %env$ae41983,%struct.ScmObj* %current_45args47549) {
%stackaddr$env-ref49108 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref49108
%stackaddr$env-ref49109 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49109
%stackaddr$env-ref49110 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 2)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref49110
%stackaddr$env-ref49111 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 3)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref49111
%stackaddr$env-ref49112 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 4)
store %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$env-ref49112
%stackaddr$env-ref49113 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 5)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref49113
%stackaddr$env-ref49114 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 6)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref49114
%stackaddr$prim49115 = alloca %struct.ScmObj*, align 8
%_95k40500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47549)
store volatile %struct.ScmObj* %_95k40500, %struct.ScmObj** %stackaddr$prim49115, align 8
%stackaddr$prim49116 = alloca %struct.ScmObj*, align 8
%current_45args47550 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47549)
store volatile %struct.ScmObj* %current_45args47550, %struct.ScmObj** %stackaddr$prim49116, align 8
%stackaddr$prim49117 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47550)
store volatile %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$prim49117, align 8
%stackaddr$makeclosure49118 = alloca %struct.ScmObj*, align 8
%fptrToInt49119 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41986 to i64
%ae41986 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt49119)
store volatile %struct.ScmObj* %ae41986, %struct.ScmObj** %stackaddr$makeclosure49118, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %f40211, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %acc40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %k40496, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %_37foldl40207, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %_37map140155, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %lsts_4340216, i64 7)
%ae41987 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49120 = alloca %struct.ScmObj*, align 8
%fptrToInt49121 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41988 to i64
%ae41988 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49121)
store volatile %struct.ScmObj* %ae41988, %struct.ScmObj** %stackaddr$makeclosure49120, align 8
%args47579$ae41986$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49122 = alloca %struct.ScmObj*, align 8
%args47579$ae41986$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41988, %struct.ScmObj* %args47579$ae41986$0)
store volatile %struct.ScmObj* %args47579$ae41986$1, %struct.ScmObj** %stackaddr$prim49122, align 8
%stackaddr$prim49123 = alloca %struct.ScmObj*, align 8
%args47579$ae41986$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41987, %struct.ScmObj* %args47579$ae41986$1)
store volatile %struct.ScmObj* %args47579$ae41986$2, %struct.ScmObj** %stackaddr$prim49123, align 8
%clofunc49124 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41986)
musttail call tailcc void %clofunc49124(%struct.ScmObj* %ae41986, %struct.ScmObj* %args47579$ae41986$2)
ret void
}

define tailcc void @proc_clo$ae41986(%struct.ScmObj* %env$ae41986,%struct.ScmObj* %current_45args47552) {
%stackaddr$env-ref49125 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref49125
%stackaddr$env-ref49126 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49126
%stackaddr$env-ref49127 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 2)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref49127
%stackaddr$env-ref49128 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 3)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref49128
%stackaddr$env-ref49129 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 4)
store %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$env-ref49129
%stackaddr$env-ref49130 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 5)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref49130
%stackaddr$env-ref49131 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 6)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref49131
%stackaddr$env-ref49132 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 7)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref49132
%stackaddr$prim49133 = alloca %struct.ScmObj*, align 8
%_95k40501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47552)
store volatile %struct.ScmObj* %_95k40501, %struct.ScmObj** %stackaddr$prim49133, align 8
%stackaddr$prim49134 = alloca %struct.ScmObj*, align 8
%current_45args47553 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47552)
store volatile %struct.ScmObj* %current_45args47553, %struct.ScmObj** %stackaddr$prim49134, align 8
%stackaddr$prim49135 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47553)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim49135, align 8
%stackaddr$makeclosure49136 = alloca %struct.ScmObj*, align 8
%fptrToInt49137 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42007 to i64
%ae42007 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49137)
store volatile %struct.ScmObj* %ae42007, %struct.ScmObj** %stackaddr$makeclosure49136, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42007, %struct.ScmObj* %f40211, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42007, %struct.ScmObj* %acc40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42007, %struct.ScmObj* %_37foldr40129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42007, %struct.ScmObj* %k40496, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae42007, %struct.ScmObj* %_37foldl40207, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae42007, %struct.ScmObj* %lsts_4340216, i64 5)
%args47574$_37map140155$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49138 = alloca %struct.ScmObj*, align 8
%args47574$_37map140155$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40209, %struct.ScmObj* %args47574$_37map140155$0)
store volatile %struct.ScmObj* %args47574$_37map140155$1, %struct.ScmObj** %stackaddr$prim49138, align 8
%stackaddr$prim49139 = alloca %struct.ScmObj*, align 8
%args47574$_37map140155$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40304, %struct.ScmObj* %args47574$_37map140155$1)
store volatile %struct.ScmObj* %args47574$_37map140155$2, %struct.ScmObj** %stackaddr$prim49139, align 8
%stackaddr$prim49140 = alloca %struct.ScmObj*, align 8
%args47574$_37map140155$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42007, %struct.ScmObj* %args47574$_37map140155$2)
store volatile %struct.ScmObj* %args47574$_37map140155$3, %struct.ScmObj** %stackaddr$prim49140, align 8
%clofunc49141 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140155)
musttail call tailcc void %clofunc49141(%struct.ScmObj* %_37map140155, %struct.ScmObj* %args47574$_37map140155$3)
ret void
}

define tailcc void @proc_clo$ae42007(%struct.ScmObj* %env$ae42007,%struct.ScmObj* %current_45args47555) {
%stackaddr$env-ref49142 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42007, i64 0)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref49142
%stackaddr$env-ref49143 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42007, i64 1)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref49143
%stackaddr$env-ref49144 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42007, i64 2)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49144
%stackaddr$env-ref49145 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42007, i64 3)
store %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$env-ref49145
%stackaddr$env-ref49146 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42007, i64 4)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref49146
%stackaddr$env-ref49147 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42007, i64 5)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref49147
%stackaddr$prim49148 = alloca %struct.ScmObj*, align 8
%_95k40502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47555)
store volatile %struct.ScmObj* %_95k40502, %struct.ScmObj** %stackaddr$prim49148, align 8
%stackaddr$prim49149 = alloca %struct.ScmObj*, align 8
%current_45args47556 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47555)
store volatile %struct.ScmObj* %current_45args47556, %struct.ScmObj** %stackaddr$prim49149, align 8
%stackaddr$prim49150 = alloca %struct.ScmObj*, align 8
%vs40214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47556)
store volatile %struct.ScmObj* %vs40214, %struct.ScmObj** %stackaddr$prim49150, align 8
%stackaddr$makeclosure49151 = alloca %struct.ScmObj*, align 8
%fptrToInt49152 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42010 to i64
%ae42010 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49152)
store volatile %struct.ScmObj* %ae42010, %struct.ScmObj** %stackaddr$makeclosure49151, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42010, %struct.ScmObj* %vs40214, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42010, %struct.ScmObj* %f40211, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42010, %struct.ScmObj* %acc40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42010, %struct.ScmObj* %_37foldr40129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae42010, %struct.ScmObj* %k40496, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae42010, %struct.ScmObj* %_37foldl40207, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae42010, %struct.ScmObj* %lsts_4340216, i64 6)
%ae42011 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49153 = alloca %struct.ScmObj*, align 8
%fptrToInt49154 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42012 to i64
%ae42012 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49154)
store volatile %struct.ScmObj* %ae42012, %struct.ScmObj** %stackaddr$makeclosure49153, align 8
%args47573$ae42010$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49155 = alloca %struct.ScmObj*, align 8
%args47573$ae42010$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42012, %struct.ScmObj* %args47573$ae42010$0)
store volatile %struct.ScmObj* %args47573$ae42010$1, %struct.ScmObj** %stackaddr$prim49155, align 8
%stackaddr$prim49156 = alloca %struct.ScmObj*, align 8
%args47573$ae42010$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42011, %struct.ScmObj* %args47573$ae42010$1)
store volatile %struct.ScmObj* %args47573$ae42010$2, %struct.ScmObj** %stackaddr$prim49156, align 8
%clofunc49157 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42010)
musttail call tailcc void %clofunc49157(%struct.ScmObj* %ae42010, %struct.ScmObj* %args47573$ae42010$2)
ret void
}

define tailcc void @proc_clo$ae42010(%struct.ScmObj* %env$ae42010,%struct.ScmObj* %current_45args47558) {
%stackaddr$env-ref49158 = alloca %struct.ScmObj*, align 8
%vs40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42010, i64 0)
store %struct.ScmObj* %vs40214, %struct.ScmObj** %stackaddr$env-ref49158
%stackaddr$env-ref49159 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42010, i64 1)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref49159
%stackaddr$env-ref49160 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42010, i64 2)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref49160
%stackaddr$env-ref49161 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42010, i64 3)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49161
%stackaddr$env-ref49162 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42010, i64 4)
store %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$env-ref49162
%stackaddr$env-ref49163 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42010, i64 5)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref49163
%stackaddr$env-ref49164 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42010, i64 6)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref49164
%stackaddr$prim49165 = alloca %struct.ScmObj*, align 8
%_95k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47558)
store volatile %struct.ScmObj* %_95k40503, %struct.ScmObj** %stackaddr$prim49165, align 8
%stackaddr$prim49166 = alloca %struct.ScmObj*, align 8
%current_45args47559 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47558)
store volatile %struct.ScmObj* %current_45args47559, %struct.ScmObj** %stackaddr$prim49166, align 8
%stackaddr$prim49167 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47559)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim49167, align 8
%ae42033 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49168 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40210, %struct.ScmObj* %ae42033)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim49168, align 8
%stackaddr$makeclosure49169 = alloca %struct.ScmObj*, align 8
%fptrToInt49170 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42035 to i64
%ae42035 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49170)
store volatile %struct.ScmObj* %ae42035, %struct.ScmObj** %stackaddr$makeclosure49169, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42035, %struct.ScmObj* %f40211, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42035, %struct.ScmObj* %k40496, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42035, %struct.ScmObj* %_37foldl40207, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42035, %struct.ScmObj* %lsts_4340216, i64 3)
%args47567$_37foldr40129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49171 = alloca %struct.ScmObj*, align 8
%args47567$_37foldr40129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40214, %struct.ScmObj* %args47567$_37foldr40129$0)
store volatile %struct.ScmObj* %args47567$_37foldr40129$1, %struct.ScmObj** %stackaddr$prim49171, align 8
%stackaddr$prim49172 = alloca %struct.ScmObj*, align 8
%args47567$_37foldr40129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40306, %struct.ScmObj* %args47567$_37foldr40129$1)
store volatile %struct.ScmObj* %args47567$_37foldr40129$2, %struct.ScmObj** %stackaddr$prim49172, align 8
%stackaddr$prim49173 = alloca %struct.ScmObj*, align 8
%args47567$_37foldr40129$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40305, %struct.ScmObj* %args47567$_37foldr40129$2)
store volatile %struct.ScmObj* %args47567$_37foldr40129$3, %struct.ScmObj** %stackaddr$prim49173, align 8
%stackaddr$prim49174 = alloca %struct.ScmObj*, align 8
%args47567$_37foldr40129$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42035, %struct.ScmObj* %args47567$_37foldr40129$3)
store volatile %struct.ScmObj* %args47567$_37foldr40129$4, %struct.ScmObj** %stackaddr$prim49174, align 8
%clofunc49175 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40129)
musttail call tailcc void %clofunc49175(%struct.ScmObj* %_37foldr40129, %struct.ScmObj* %args47567$_37foldr40129$4)
ret void
}

define tailcc void @proc_clo$ae42035(%struct.ScmObj* %env$ae42035,%struct.ScmObj* %current_45args47561) {
%stackaddr$env-ref49176 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42035, i64 0)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref49176
%stackaddr$env-ref49177 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42035, i64 1)
store %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$env-ref49177
%stackaddr$env-ref49178 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42035, i64 2)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref49178
%stackaddr$env-ref49179 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42035, i64 3)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref49179
%stackaddr$prim49180 = alloca %struct.ScmObj*, align 8
%_95k40504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47561)
store volatile %struct.ScmObj* %_95k40504, %struct.ScmObj** %stackaddr$prim49180, align 8
%stackaddr$prim49181 = alloca %struct.ScmObj*, align 8
%current_45args47562 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47561)
store volatile %struct.ScmObj* %current_45args47562, %struct.ScmObj** %stackaddr$prim49181, align 8
%stackaddr$prim49182 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47562)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim49182, align 8
%stackaddr$makeclosure49183 = alloca %struct.ScmObj*, align 8
%fptrToInt49184 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42039 to i64
%ae42039 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49184)
store volatile %struct.ScmObj* %ae42039, %struct.ScmObj** %stackaddr$makeclosure49183, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42039, %struct.ScmObj* %f40211, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42039, %struct.ScmObj* %k40496, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42039, %struct.ScmObj* %_37foldl40207, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42039, %struct.ScmObj* %lsts_4340216, i64 3)
%stackaddr$prim49185 = alloca %struct.ScmObj*, align 8
%cpsargs40507 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42039, %struct.ScmObj* %anf_45bind40307)
store volatile %struct.ScmObj* %cpsargs40507, %struct.ScmObj** %stackaddr$prim49185, align 8
%clofunc49186 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40211)
musttail call tailcc void %clofunc49186(%struct.ScmObj* %f40211, %struct.ScmObj* %cpsargs40507)
ret void
}

define tailcc void @proc_clo$ae42039(%struct.ScmObj* %env$ae42039,%struct.ScmObj* %current_45args47564) {
%stackaddr$env-ref49187 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42039, i64 0)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref49187
%stackaddr$env-ref49188 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42039, i64 1)
store %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$env-ref49188
%stackaddr$env-ref49189 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42039, i64 2)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref49189
%stackaddr$env-ref49190 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42039, i64 3)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref49190
%stackaddr$prim49191 = alloca %struct.ScmObj*, align 8
%_95k40505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47564)
store volatile %struct.ScmObj* %_95k40505, %struct.ScmObj** %stackaddr$prim49191, align 8
%stackaddr$prim49192 = alloca %struct.ScmObj*, align 8
%current_45args47565 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47564)
store volatile %struct.ScmObj* %current_45args47565, %struct.ScmObj** %stackaddr$prim49192, align 8
%stackaddr$prim49193 = alloca %struct.ScmObj*, align 8
%acc_4340218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47565)
store volatile %struct.ScmObj* %acc_4340218, %struct.ScmObj** %stackaddr$prim49193, align 8
%stackaddr$prim49194 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340218, %struct.ScmObj* %lsts_4340216)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim49194, align 8
%stackaddr$prim49195 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40211, %struct.ScmObj* %anf_45bind40308)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim49195, align 8
%stackaddr$prim49196 = alloca %struct.ScmObj*, align 8
%cpsargs40506 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40496, %struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %cpsargs40506, %struct.ScmObj** %stackaddr$prim49196, align 8
%clofunc49197 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40207)
musttail call tailcc void %clofunc49197(%struct.ScmObj* %_37foldl40207, %struct.ScmObj* %cpsargs40506)
ret void
}

define tailcc void @proc_clo$ae42012(%struct.ScmObj* %env$ae42012,%struct.ScmObj* %current_45args47568) {
%stackaddr$prim49198 = alloca %struct.ScmObj*, align 8
%k40508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47568)
store volatile %struct.ScmObj* %k40508, %struct.ScmObj** %stackaddr$prim49198, align 8
%stackaddr$prim49199 = alloca %struct.ScmObj*, align 8
%current_45args47569 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47568)
store volatile %struct.ScmObj* %current_45args47569, %struct.ScmObj** %stackaddr$prim49199, align 8
%stackaddr$prim49200 = alloca %struct.ScmObj*, align 8
%a40220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47569)
store volatile %struct.ScmObj* %a40220, %struct.ScmObj** %stackaddr$prim49200, align 8
%stackaddr$prim49201 = alloca %struct.ScmObj*, align 8
%current_45args47570 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47569)
store volatile %struct.ScmObj* %current_45args47570, %struct.ScmObj** %stackaddr$prim49201, align 8
%stackaddr$prim49202 = alloca %struct.ScmObj*, align 8
%b40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47570)
store volatile %struct.ScmObj* %b40219, %struct.ScmObj** %stackaddr$prim49202, align 8
%stackaddr$prim49203 = alloca %struct.ScmObj*, align 8
%cpsprim40509 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40220, %struct.ScmObj* %b40219)
store volatile %struct.ScmObj* %cpsprim40509, %struct.ScmObj** %stackaddr$prim49203, align 8
%ae42016 = call %struct.ScmObj* @const_init_int(i64 0)
%args47572$k40508$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49204 = alloca %struct.ScmObj*, align 8
%args47572$k40508$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40509, %struct.ScmObj* %args47572$k40508$0)
store volatile %struct.ScmObj* %args47572$k40508$1, %struct.ScmObj** %stackaddr$prim49204, align 8
%stackaddr$prim49205 = alloca %struct.ScmObj*, align 8
%args47572$k40508$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42016, %struct.ScmObj* %args47572$k40508$1)
store volatile %struct.ScmObj* %args47572$k40508$2, %struct.ScmObj** %stackaddr$prim49205, align 8
%clofunc49206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40508)
musttail call tailcc void %clofunc49206(%struct.ScmObj* %k40508, %struct.ScmObj* %args47572$k40508$2)
ret void
}

define tailcc void @proc_clo$ae41988(%struct.ScmObj* %env$ae41988,%struct.ScmObj* %current_45args47575) {
%stackaddr$prim49207 = alloca %struct.ScmObj*, align 8
%k40510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47575)
store volatile %struct.ScmObj* %k40510, %struct.ScmObj** %stackaddr$prim49207, align 8
%stackaddr$prim49208 = alloca %struct.ScmObj*, align 8
%current_45args47576 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47575)
store volatile %struct.ScmObj* %current_45args47576, %struct.ScmObj** %stackaddr$prim49208, align 8
%stackaddr$prim49209 = alloca %struct.ScmObj*, align 8
%x40215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47576)
store volatile %struct.ScmObj* %x40215, %struct.ScmObj** %stackaddr$prim49209, align 8
%stackaddr$prim49210 = alloca %struct.ScmObj*, align 8
%cpsprim40511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40215)
store volatile %struct.ScmObj* %cpsprim40511, %struct.ScmObj** %stackaddr$prim49210, align 8
%ae41991 = call %struct.ScmObj* @const_init_int(i64 0)
%args47578$k40510$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49211 = alloca %struct.ScmObj*, align 8
%args47578$k40510$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40511, %struct.ScmObj* %args47578$k40510$0)
store volatile %struct.ScmObj* %args47578$k40510$1, %struct.ScmObj** %stackaddr$prim49211, align 8
%stackaddr$prim49212 = alloca %struct.ScmObj*, align 8
%args47578$k40510$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41991, %struct.ScmObj* %args47578$k40510$1)
store volatile %struct.ScmObj* %args47578$k40510$2, %struct.ScmObj** %stackaddr$prim49212, align 8
%clofunc49213 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40510)
musttail call tailcc void %clofunc49213(%struct.ScmObj* %k40510, %struct.ScmObj* %args47578$k40510$2)
ret void
}

define tailcc void @proc_clo$ae41964(%struct.ScmObj* %env$ae41964,%struct.ScmObj* %current_45args47581) {
%stackaddr$prim49214 = alloca %struct.ScmObj*, align 8
%k40512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47581)
store volatile %struct.ScmObj* %k40512, %struct.ScmObj** %stackaddr$prim49214, align 8
%stackaddr$prim49215 = alloca %struct.ScmObj*, align 8
%current_45args47582 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47581)
store volatile %struct.ScmObj* %current_45args47582, %struct.ScmObj** %stackaddr$prim49215, align 8
%stackaddr$prim49216 = alloca %struct.ScmObj*, align 8
%x40217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47582)
store volatile %struct.ScmObj* %x40217, %struct.ScmObj** %stackaddr$prim49216, align 8
%stackaddr$prim49217 = alloca %struct.ScmObj*, align 8
%cpsprim40513 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40217)
store volatile %struct.ScmObj* %cpsprim40513, %struct.ScmObj** %stackaddr$prim49217, align 8
%ae41967 = call %struct.ScmObj* @const_init_int(i64 0)
%args47584$k40512$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49218 = alloca %struct.ScmObj*, align 8
%args47584$k40512$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40513, %struct.ScmObj* %args47584$k40512$0)
store volatile %struct.ScmObj* %args47584$k40512$1, %struct.ScmObj** %stackaddr$prim49218, align 8
%stackaddr$prim49219 = alloca %struct.ScmObj*, align 8
%args47584$k40512$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41967, %struct.ScmObj* %args47584$k40512$1)
store volatile %struct.ScmObj* %args47584$k40512$2, %struct.ScmObj** %stackaddr$prim49219, align 8
%clofunc49220 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40512)
musttail call tailcc void %clofunc49220(%struct.ScmObj* %k40512, %struct.ScmObj* %args47584$k40512$2)
ret void
}

define tailcc void @proc_clo$ae41916(%struct.ScmObj* %env$ae41916,%struct.ScmObj* %current_45args47587) {
%stackaddr$prim49221 = alloca %struct.ScmObj*, align 8
%k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47587)
store volatile %struct.ScmObj* %k40514, %struct.ScmObj** %stackaddr$prim49221, align 8
%stackaddr$prim49222 = alloca %struct.ScmObj*, align 8
%current_45args47588 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47587)
store volatile %struct.ScmObj* %current_45args47588, %struct.ScmObj** %stackaddr$prim49222, align 8
%stackaddr$prim49223 = alloca %struct.ScmObj*, align 8
%lst40213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47588)
store volatile %struct.ScmObj* %lst40213, %struct.ScmObj** %stackaddr$prim49223, align 8
%stackaddr$prim49224 = alloca %struct.ScmObj*, align 8
%current_45args47589 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47588)
store volatile %struct.ScmObj* %current_45args47589, %struct.ScmObj** %stackaddr$prim49224, align 8
%stackaddr$prim49225 = alloca %struct.ScmObj*, align 8
%b40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47589)
store volatile %struct.ScmObj* %b40212, %struct.ScmObj** %stackaddr$prim49225, align 8
%truthy$cmp49226 = call i64 @is_truthy_value(%struct.ScmObj* %b40212)
%cmp$cmp49226 = icmp eq i64 %truthy$cmp49226, 1
br i1 %cmp$cmp49226, label %truebranch$cmp49226, label %falsebranch$cmp49226
truebranch$cmp49226:
%ae41919 = call %struct.ScmObj* @const_init_int(i64 0)
%args47591$k40514$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49227 = alloca %struct.ScmObj*, align 8
%args47591$k40514$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40212, %struct.ScmObj* %args47591$k40514$0)
store volatile %struct.ScmObj* %args47591$k40514$1, %struct.ScmObj** %stackaddr$prim49227, align 8
%stackaddr$prim49228 = alloca %struct.ScmObj*, align 8
%args47591$k40514$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41919, %struct.ScmObj* %args47591$k40514$1)
store volatile %struct.ScmObj* %args47591$k40514$2, %struct.ScmObj** %stackaddr$prim49228, align 8
%clofunc49229 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40514)
musttail call tailcc void %clofunc49229(%struct.ScmObj* %k40514, %struct.ScmObj* %args47591$k40514$2)
ret void
falsebranch$cmp49226:
%stackaddr$prim49230 = alloca %struct.ScmObj*, align 8
%cpsprim40515 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40213)
store volatile %struct.ScmObj* %cpsprim40515, %struct.ScmObj** %stackaddr$prim49230, align 8
%ae41926 = call %struct.ScmObj* @const_init_int(i64 0)
%args47592$k40514$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49231 = alloca %struct.ScmObj*, align 8
%args47592$k40514$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40515, %struct.ScmObj* %args47592$k40514$0)
store volatile %struct.ScmObj* %args47592$k40514$1, %struct.ScmObj** %stackaddr$prim49231, align 8
%stackaddr$prim49232 = alloca %struct.ScmObj*, align 8
%args47592$k40514$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41926, %struct.ScmObj* %args47592$k40514$1)
store volatile %struct.ScmObj* %args47592$k40514$2, %struct.ScmObj** %stackaddr$prim49232, align 8
%clofunc49233 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40514)
musttail call tailcc void %clofunc49233(%struct.ScmObj* %k40514, %struct.ScmObj* %args47592$k40514$2)
ret void
}

define tailcc void @proc_clo$ae41757(%struct.ScmObj* %env$ae41757,%struct.ScmObj* %args4015140516) {
%stackaddr$env-ref49234 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41757, i64 0)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref49234
%stackaddr$env-ref49235 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41757, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49235
%stackaddr$env-ref49236 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41757, i64 2)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref49236
%stackaddr$prim49237 = alloca %struct.ScmObj*, align 8
%k40517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015140516)
store volatile %struct.ScmObj* %k40517, %struct.ScmObj** %stackaddr$prim49237, align 8
%stackaddr$prim49238 = alloca %struct.ScmObj*, align 8
%args40151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015140516)
store volatile %struct.ScmObj* %args40151, %struct.ScmObj** %stackaddr$prim49238, align 8
%stackaddr$prim49239 = alloca %struct.ScmObj*, align 8
%f40153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40151)
store volatile %struct.ScmObj* %f40153, %struct.ScmObj** %stackaddr$prim49239, align 8
%stackaddr$prim49240 = alloca %struct.ScmObj*, align 8
%lsts40152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40151)
store volatile %struct.ScmObj* %lsts40152, %struct.ScmObj** %stackaddr$prim49240, align 8
%stackaddr$makeclosure49241 = alloca %struct.ScmObj*, align 8
%fptrToInt49242 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41762 to i64
%ae41762 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49242)
store volatile %struct.ScmObj* %ae41762, %struct.ScmObj** %stackaddr$makeclosure49241, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41762, %struct.ScmObj* %k40517, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41762, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41762, %struct.ScmObj* %lsts40152, i64 2)
%ae41763 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49243 = alloca %struct.ScmObj*, align 8
%fptrToInt49244 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41764 to i64
%ae41764 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49244)
store volatile %struct.ScmObj* %ae41764, %struct.ScmObj** %stackaddr$makeclosure49243, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41764, %struct.ScmObj* %_37last40146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41764, %struct.ScmObj* %_37drop_45right40143, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41764, %struct.ScmObj* %f40153, i64 2)
%args47611$ae41762$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49245 = alloca %struct.ScmObj*, align 8
%args47611$ae41762$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41764, %struct.ScmObj* %args47611$ae41762$0)
store volatile %struct.ScmObj* %args47611$ae41762$1, %struct.ScmObj** %stackaddr$prim49245, align 8
%stackaddr$prim49246 = alloca %struct.ScmObj*, align 8
%args47611$ae41762$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41763, %struct.ScmObj* %args47611$ae41762$1)
store volatile %struct.ScmObj* %args47611$ae41762$2, %struct.ScmObj** %stackaddr$prim49246, align 8
%clofunc49247 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41762)
musttail call tailcc void %clofunc49247(%struct.ScmObj* %ae41762, %struct.ScmObj* %args47611$ae41762$2)
ret void
}

define tailcc void @proc_clo$ae41762(%struct.ScmObj* %env$ae41762,%struct.ScmObj* %current_45args47596) {
%stackaddr$env-ref49248 = alloca %struct.ScmObj*, align 8
%k40517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41762, i64 0)
store %struct.ScmObj* %k40517, %struct.ScmObj** %stackaddr$env-ref49248
%stackaddr$env-ref49249 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41762, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49249
%stackaddr$env-ref49250 = alloca %struct.ScmObj*, align 8
%lsts40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41762, i64 2)
store %struct.ScmObj* %lsts40152, %struct.ScmObj** %stackaddr$env-ref49250
%stackaddr$prim49251 = alloca %struct.ScmObj*, align 8
%_95k40518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47596)
store volatile %struct.ScmObj* %_95k40518, %struct.ScmObj** %stackaddr$prim49251, align 8
%stackaddr$prim49252 = alloca %struct.ScmObj*, align 8
%current_45args47597 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47596)
store volatile %struct.ScmObj* %current_45args47597, %struct.ScmObj** %stackaddr$prim49252, align 8
%stackaddr$prim49253 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47597)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim49253, align 8
%ae41825 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49254 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41825, %struct.ScmObj* %lsts40152)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim49254, align 8
%stackaddr$prim49255 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40296, %struct.ScmObj* %anf_45bind40297)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim49255, align 8
%stackaddr$prim49256 = alloca %struct.ScmObj*, align 8
%cpsargs40519 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40517, %struct.ScmObj* %anf_45bind40298)
store volatile %struct.ScmObj* %cpsargs40519, %struct.ScmObj** %stackaddr$prim49256, align 8
%clofunc49257 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40129)
musttail call tailcc void %clofunc49257(%struct.ScmObj* %_37foldr40129, %struct.ScmObj* %cpsargs40519)
ret void
}

define tailcc void @proc_clo$ae41764(%struct.ScmObj* %env$ae41764,%struct.ScmObj* %fargs4015440520) {
%stackaddr$env-ref49258 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41764, i64 0)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref49258
%stackaddr$env-ref49259 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41764, i64 1)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref49259
%stackaddr$env-ref49260 = alloca %struct.ScmObj*, align 8
%f40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41764, i64 2)
store %struct.ScmObj* %f40153, %struct.ScmObj** %stackaddr$env-ref49260
%stackaddr$prim49261 = alloca %struct.ScmObj*, align 8
%k40521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4015440520)
store volatile %struct.ScmObj* %k40521, %struct.ScmObj** %stackaddr$prim49261, align 8
%stackaddr$prim49262 = alloca %struct.ScmObj*, align 8
%fargs40154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4015440520)
store volatile %struct.ScmObj* %fargs40154, %struct.ScmObj** %stackaddr$prim49262, align 8
%stackaddr$makeclosure49263 = alloca %struct.ScmObj*, align 8
%fptrToInt49264 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41768 to i64
%ae41768 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49264)
store volatile %struct.ScmObj* %ae41768, %struct.ScmObj** %stackaddr$makeclosure49263, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41768, %struct.ScmObj* %k40521, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41768, %struct.ScmObj* %f40153, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41768, %struct.ScmObj* %_37last40146, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41768, %struct.ScmObj* %fargs40154, i64 3)
%ae41770 = call %struct.ScmObj* @const_init_int(i64 1)
%args47610$_37drop_45right40143$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49265 = alloca %struct.ScmObj*, align 8
%args47610$_37drop_45right40143$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41770, %struct.ScmObj* %args47610$_37drop_45right40143$0)
store volatile %struct.ScmObj* %args47610$_37drop_45right40143$1, %struct.ScmObj** %stackaddr$prim49265, align 8
%stackaddr$prim49266 = alloca %struct.ScmObj*, align 8
%args47610$_37drop_45right40143$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40154, %struct.ScmObj* %args47610$_37drop_45right40143$1)
store volatile %struct.ScmObj* %args47610$_37drop_45right40143$2, %struct.ScmObj** %stackaddr$prim49266, align 8
%stackaddr$prim49267 = alloca %struct.ScmObj*, align 8
%args47610$_37drop_45right40143$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41768, %struct.ScmObj* %args47610$_37drop_45right40143$2)
store volatile %struct.ScmObj* %args47610$_37drop_45right40143$3, %struct.ScmObj** %stackaddr$prim49267, align 8
%clofunc49268 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40143)
musttail call tailcc void %clofunc49268(%struct.ScmObj* %_37drop_45right40143, %struct.ScmObj* %args47610$_37drop_45right40143$3)
ret void
}

define tailcc void @proc_clo$ae41768(%struct.ScmObj* %env$ae41768,%struct.ScmObj* %current_45args47599) {
%stackaddr$env-ref49269 = alloca %struct.ScmObj*, align 8
%k40521 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41768, i64 0)
store %struct.ScmObj* %k40521, %struct.ScmObj** %stackaddr$env-ref49269
%stackaddr$env-ref49270 = alloca %struct.ScmObj*, align 8
%f40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41768, i64 1)
store %struct.ScmObj* %f40153, %struct.ScmObj** %stackaddr$env-ref49270
%stackaddr$env-ref49271 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41768, i64 2)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref49271
%stackaddr$env-ref49272 = alloca %struct.ScmObj*, align 8
%fargs40154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41768, i64 3)
store %struct.ScmObj* %fargs40154, %struct.ScmObj** %stackaddr$env-ref49272
%stackaddr$prim49273 = alloca %struct.ScmObj*, align 8
%_95k40522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47599)
store volatile %struct.ScmObj* %_95k40522, %struct.ScmObj** %stackaddr$prim49273, align 8
%stackaddr$prim49274 = alloca %struct.ScmObj*, align 8
%current_45args47600 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47599)
store volatile %struct.ScmObj* %current_45args47600, %struct.ScmObj** %stackaddr$prim49274, align 8
%stackaddr$prim49275 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47600)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim49275, align 8
%stackaddr$makeclosure49276 = alloca %struct.ScmObj*, align 8
%fptrToInt49277 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41775 to i64
%ae41775 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49277)
store volatile %struct.ScmObj* %ae41775, %struct.ScmObj** %stackaddr$makeclosure49276, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41775, %struct.ScmObj* %_37last40146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41775, %struct.ScmObj* %fargs40154, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41775, %struct.ScmObj* %k40521, i64 2)
%stackaddr$prim49278 = alloca %struct.ScmObj*, align 8
%cpsargs40526 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41775, %struct.ScmObj* %anf_45bind40293)
store volatile %struct.ScmObj* %cpsargs40526, %struct.ScmObj** %stackaddr$prim49278, align 8
%clofunc49279 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40153)
musttail call tailcc void %clofunc49279(%struct.ScmObj* %f40153, %struct.ScmObj* %cpsargs40526)
ret void
}

define tailcc void @proc_clo$ae41775(%struct.ScmObj* %env$ae41775,%struct.ScmObj* %current_45args47602) {
%stackaddr$env-ref49280 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41775, i64 0)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref49280
%stackaddr$env-ref49281 = alloca %struct.ScmObj*, align 8
%fargs40154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41775, i64 1)
store %struct.ScmObj* %fargs40154, %struct.ScmObj** %stackaddr$env-ref49281
%stackaddr$env-ref49282 = alloca %struct.ScmObj*, align 8
%k40521 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41775, i64 2)
store %struct.ScmObj* %k40521, %struct.ScmObj** %stackaddr$env-ref49282
%stackaddr$prim49283 = alloca %struct.ScmObj*, align 8
%_95k40523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47602)
store volatile %struct.ScmObj* %_95k40523, %struct.ScmObj** %stackaddr$prim49283, align 8
%stackaddr$prim49284 = alloca %struct.ScmObj*, align 8
%current_45args47603 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47602)
store volatile %struct.ScmObj* %current_45args47603, %struct.ScmObj** %stackaddr$prim49284, align 8
%stackaddr$prim49285 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47603)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim49285, align 8
%stackaddr$makeclosure49286 = alloca %struct.ScmObj*, align 8
%fptrToInt49287 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41780 to i64
%ae41780 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49287)
store volatile %struct.ScmObj* %ae41780, %struct.ScmObj** %stackaddr$makeclosure49286, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41780, %struct.ScmObj* %anf_45bind40294, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41780, %struct.ScmObj* %k40521, i64 1)
%args47609$_37last40146$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49288 = alloca %struct.ScmObj*, align 8
%args47609$_37last40146$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40154, %struct.ScmObj* %args47609$_37last40146$0)
store volatile %struct.ScmObj* %args47609$_37last40146$1, %struct.ScmObj** %stackaddr$prim49288, align 8
%stackaddr$prim49289 = alloca %struct.ScmObj*, align 8
%args47609$_37last40146$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41780, %struct.ScmObj* %args47609$_37last40146$1)
store volatile %struct.ScmObj* %args47609$_37last40146$2, %struct.ScmObj** %stackaddr$prim49289, align 8
%clofunc49290 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40146)
musttail call tailcc void %clofunc49290(%struct.ScmObj* %_37last40146, %struct.ScmObj* %args47609$_37last40146$2)
ret void
}

define tailcc void @proc_clo$ae41780(%struct.ScmObj* %env$ae41780,%struct.ScmObj* %current_45args47605) {
%stackaddr$env-ref49291 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41780, i64 0)
store %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$env-ref49291
%stackaddr$env-ref49292 = alloca %struct.ScmObj*, align 8
%k40521 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41780, i64 1)
store %struct.ScmObj* %k40521, %struct.ScmObj** %stackaddr$env-ref49292
%stackaddr$prim49293 = alloca %struct.ScmObj*, align 8
%_95k40524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47605)
store volatile %struct.ScmObj* %_95k40524, %struct.ScmObj** %stackaddr$prim49293, align 8
%stackaddr$prim49294 = alloca %struct.ScmObj*, align 8
%current_45args47606 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47605)
store volatile %struct.ScmObj* %current_45args47606, %struct.ScmObj** %stackaddr$prim49294, align 8
%stackaddr$prim49295 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47606)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim49295, align 8
%stackaddr$prim49296 = alloca %struct.ScmObj*, align 8
%cpsprim40525 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40294, %struct.ScmObj* %anf_45bind40295)
store volatile %struct.ScmObj* %cpsprim40525, %struct.ScmObj** %stackaddr$prim49296, align 8
%ae41785 = call %struct.ScmObj* @const_init_int(i64 0)
%args47608$k40521$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49297 = alloca %struct.ScmObj*, align 8
%args47608$k40521$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40525, %struct.ScmObj* %args47608$k40521$0)
store volatile %struct.ScmObj* %args47608$k40521$1, %struct.ScmObj** %stackaddr$prim49297, align 8
%stackaddr$prim49298 = alloca %struct.ScmObj*, align 8
%args47608$k40521$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41785, %struct.ScmObj* %args47608$k40521$1)
store volatile %struct.ScmObj* %args47608$k40521$2, %struct.ScmObj** %stackaddr$prim49298, align 8
%clofunc49299 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40521)
musttail call tailcc void %clofunc49299(%struct.ScmObj* %k40521, %struct.ScmObj* %args47608$k40521$2)
ret void
}

define tailcc void @proc_clo$ae41680(%struct.ScmObj* %env$ae41680,%struct.ScmObj* %current_45args47613) {
%stackaddr$env-ref49300 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41680, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49300
%stackaddr$prim49301 = alloca %struct.ScmObj*, align 8
%k40527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47613)
store volatile %struct.ScmObj* %k40527, %struct.ScmObj** %stackaddr$prim49301, align 8
%stackaddr$prim49302 = alloca %struct.ScmObj*, align 8
%current_45args47614 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47613)
store volatile %struct.ScmObj* %current_45args47614, %struct.ScmObj** %stackaddr$prim49302, align 8
%stackaddr$prim49303 = alloca %struct.ScmObj*, align 8
%f40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47614)
store volatile %struct.ScmObj* %f40157, %struct.ScmObj** %stackaddr$prim49303, align 8
%stackaddr$prim49304 = alloca %struct.ScmObj*, align 8
%current_45args47615 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47614)
store volatile %struct.ScmObj* %current_45args47615, %struct.ScmObj** %stackaddr$prim49304, align 8
%stackaddr$prim49305 = alloca %struct.ScmObj*, align 8
%lst40156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47615)
store volatile %struct.ScmObj* %lst40156, %struct.ScmObj** %stackaddr$prim49305, align 8
%stackaddr$makeclosure49306 = alloca %struct.ScmObj*, align 8
%fptrToInt49307 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41681 to i64
%ae41681 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49307)
store volatile %struct.ScmObj* %ae41681, %struct.ScmObj** %stackaddr$makeclosure49306, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41681, %struct.ScmObj* %lst40156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41681, %struct.ScmObj* %_37foldr140124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41681, %struct.ScmObj* %k40527, i64 2)
%ae41682 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49308 = alloca %struct.ScmObj*, align 8
%fptrToInt49309 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41683 to i64
%ae41683 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49309)
store volatile %struct.ScmObj* %ae41683, %struct.ScmObj** %stackaddr$makeclosure49308, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41683, %struct.ScmObj* %f40157, i64 0)
%args47630$ae41681$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49310 = alloca %struct.ScmObj*, align 8
%args47630$ae41681$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41683, %struct.ScmObj* %args47630$ae41681$0)
store volatile %struct.ScmObj* %args47630$ae41681$1, %struct.ScmObj** %stackaddr$prim49310, align 8
%stackaddr$prim49311 = alloca %struct.ScmObj*, align 8
%args47630$ae41681$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41682, %struct.ScmObj* %args47630$ae41681$1)
store volatile %struct.ScmObj* %args47630$ae41681$2, %struct.ScmObj** %stackaddr$prim49311, align 8
%clofunc49312 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41681)
musttail call tailcc void %clofunc49312(%struct.ScmObj* %ae41681, %struct.ScmObj* %args47630$ae41681$2)
ret void
}

define tailcc void @proc_clo$ae41681(%struct.ScmObj* %env$ae41681,%struct.ScmObj* %current_45args47617) {
%stackaddr$env-ref49313 = alloca %struct.ScmObj*, align 8
%lst40156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41681, i64 0)
store %struct.ScmObj* %lst40156, %struct.ScmObj** %stackaddr$env-ref49313
%stackaddr$env-ref49314 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41681, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49314
%stackaddr$env-ref49315 = alloca %struct.ScmObj*, align 8
%k40527 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41681, i64 2)
store %struct.ScmObj* %k40527, %struct.ScmObj** %stackaddr$env-ref49315
%stackaddr$prim49316 = alloca %struct.ScmObj*, align 8
%_95k40528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47617)
store volatile %struct.ScmObj* %_95k40528, %struct.ScmObj** %stackaddr$prim49316, align 8
%stackaddr$prim49317 = alloca %struct.ScmObj*, align 8
%current_45args47618 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47617)
store volatile %struct.ScmObj* %current_45args47618, %struct.ScmObj** %stackaddr$prim49317, align 8
%stackaddr$prim49318 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47618)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim49318, align 8
%ae41715 = call %struct.ScmObj* @const_init_null()
%args47620$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49319 = alloca %struct.ScmObj*, align 8
%args47620$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40156, %struct.ScmObj* %args47620$_37foldr140124$0)
store volatile %struct.ScmObj* %args47620$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim49319, align 8
%stackaddr$prim49320 = alloca %struct.ScmObj*, align 8
%args47620$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41715, %struct.ScmObj* %args47620$_37foldr140124$1)
store volatile %struct.ScmObj* %args47620$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim49320, align 8
%stackaddr$prim49321 = alloca %struct.ScmObj*, align 8
%args47620$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40292, %struct.ScmObj* %args47620$_37foldr140124$2)
store volatile %struct.ScmObj* %args47620$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim49321, align 8
%stackaddr$prim49322 = alloca %struct.ScmObj*, align 8
%args47620$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40527, %struct.ScmObj* %args47620$_37foldr140124$3)
store volatile %struct.ScmObj* %args47620$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim49322, align 8
%clofunc49323 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc49323(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args47620$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae41683(%struct.ScmObj* %env$ae41683,%struct.ScmObj* %current_45args47621) {
%stackaddr$env-ref49324 = alloca %struct.ScmObj*, align 8
%f40157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41683, i64 0)
store %struct.ScmObj* %f40157, %struct.ScmObj** %stackaddr$env-ref49324
%stackaddr$prim49325 = alloca %struct.ScmObj*, align 8
%k40529 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47621)
store volatile %struct.ScmObj* %k40529, %struct.ScmObj** %stackaddr$prim49325, align 8
%stackaddr$prim49326 = alloca %struct.ScmObj*, align 8
%current_45args47622 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47621)
store volatile %struct.ScmObj* %current_45args47622, %struct.ScmObj** %stackaddr$prim49326, align 8
%stackaddr$prim49327 = alloca %struct.ScmObj*, align 8
%v40159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47622)
store volatile %struct.ScmObj* %v40159, %struct.ScmObj** %stackaddr$prim49327, align 8
%stackaddr$prim49328 = alloca %struct.ScmObj*, align 8
%current_45args47623 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47622)
store volatile %struct.ScmObj* %current_45args47623, %struct.ScmObj** %stackaddr$prim49328, align 8
%stackaddr$prim49329 = alloca %struct.ScmObj*, align 8
%r40158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47623)
store volatile %struct.ScmObj* %r40158, %struct.ScmObj** %stackaddr$prim49329, align 8
%stackaddr$makeclosure49330 = alloca %struct.ScmObj*, align 8
%fptrToInt49331 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41685 to i64
%ae41685 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49331)
store volatile %struct.ScmObj* %ae41685, %struct.ScmObj** %stackaddr$makeclosure49330, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41685, %struct.ScmObj* %k40529, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41685, %struct.ScmObj* %r40158, i64 1)
%args47629$f40157$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49332 = alloca %struct.ScmObj*, align 8
%args47629$f40157$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40159, %struct.ScmObj* %args47629$f40157$0)
store volatile %struct.ScmObj* %args47629$f40157$1, %struct.ScmObj** %stackaddr$prim49332, align 8
%stackaddr$prim49333 = alloca %struct.ScmObj*, align 8
%args47629$f40157$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41685, %struct.ScmObj* %args47629$f40157$1)
store volatile %struct.ScmObj* %args47629$f40157$2, %struct.ScmObj** %stackaddr$prim49333, align 8
%clofunc49334 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40157)
musttail call tailcc void %clofunc49334(%struct.ScmObj* %f40157, %struct.ScmObj* %args47629$f40157$2)
ret void
}

define tailcc void @proc_clo$ae41685(%struct.ScmObj* %env$ae41685,%struct.ScmObj* %current_45args47625) {
%stackaddr$env-ref49335 = alloca %struct.ScmObj*, align 8
%k40529 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41685, i64 0)
store %struct.ScmObj* %k40529, %struct.ScmObj** %stackaddr$env-ref49335
%stackaddr$env-ref49336 = alloca %struct.ScmObj*, align 8
%r40158 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41685, i64 1)
store %struct.ScmObj* %r40158, %struct.ScmObj** %stackaddr$env-ref49336
%stackaddr$prim49337 = alloca %struct.ScmObj*, align 8
%_95k40530 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47625)
store volatile %struct.ScmObj* %_95k40530, %struct.ScmObj** %stackaddr$prim49337, align 8
%stackaddr$prim49338 = alloca %struct.ScmObj*, align 8
%current_45args47626 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47625)
store volatile %struct.ScmObj* %current_45args47626, %struct.ScmObj** %stackaddr$prim49338, align 8
%stackaddr$prim49339 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47626)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim49339, align 8
%stackaddr$prim49340 = alloca %struct.ScmObj*, align 8
%cpsprim40531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40291, %struct.ScmObj* %r40158)
store volatile %struct.ScmObj* %cpsprim40531, %struct.ScmObj** %stackaddr$prim49340, align 8
%ae41690 = call %struct.ScmObj* @const_init_int(i64 0)
%args47628$k40529$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49341 = alloca %struct.ScmObj*, align 8
%args47628$k40529$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40531, %struct.ScmObj* %args47628$k40529$0)
store volatile %struct.ScmObj* %args47628$k40529$1, %struct.ScmObj** %stackaddr$prim49341, align 8
%stackaddr$prim49342 = alloca %struct.ScmObj*, align 8
%args47628$k40529$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41690, %struct.ScmObj* %args47628$k40529$1)
store volatile %struct.ScmObj* %args47628$k40529$2, %struct.ScmObj** %stackaddr$prim49342, align 8
%clofunc49343 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40529)
musttail call tailcc void %clofunc49343(%struct.ScmObj* %k40529, %struct.ScmObj* %args47628$k40529$2)
ret void
}

define tailcc void @proc_clo$ae41294(%struct.ScmObj* %env$ae41294,%struct.ScmObj* %current_45args47633) {
%stackaddr$env-ref49344 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49344
%stackaddr$env-ref49345 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 1)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref49345
%stackaddr$prim49346 = alloca %struct.ScmObj*, align 8
%k40532 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47633)
store volatile %struct.ScmObj* %k40532, %struct.ScmObj** %stackaddr$prim49346, align 8
%stackaddr$prim49347 = alloca %struct.ScmObj*, align 8
%current_45args47634 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47633)
store volatile %struct.ScmObj* %current_45args47634, %struct.ScmObj** %stackaddr$prim49347, align 8
%stackaddr$prim49348 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47634)
store volatile %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$prim49348, align 8
%ae41296 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49349 = alloca %struct.ScmObj*, align 8
%fptrToInt49350 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41297 to i64
%ae41297 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49350)
store volatile %struct.ScmObj* %ae41297, %struct.ScmObj** %stackaddr$makeclosure49349, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %_37foldr40130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %_37foldr140124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %_37map140120, i64 2)
%args47691$k40532$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49351 = alloca %struct.ScmObj*, align 8
%args47691$k40532$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41297, %struct.ScmObj* %args47691$k40532$0)
store volatile %struct.ScmObj* %args47691$k40532$1, %struct.ScmObj** %stackaddr$prim49351, align 8
%stackaddr$prim49352 = alloca %struct.ScmObj*, align 8
%args47691$k40532$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41296, %struct.ScmObj* %args47691$k40532$1)
store volatile %struct.ScmObj* %args47691$k40532$2, %struct.ScmObj** %stackaddr$prim49352, align 8
%clofunc49353 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40532)
musttail call tailcc void %clofunc49353(%struct.ScmObj* %k40532, %struct.ScmObj* %args47691$k40532$2)
ret void
}

define tailcc void @proc_clo$ae41297(%struct.ScmObj* %env$ae41297,%struct.ScmObj* %args4013140533) {
%stackaddr$env-ref49354 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 0)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref49354
%stackaddr$env-ref49355 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49355
%stackaddr$env-ref49356 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 2)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref49356
%stackaddr$prim49357 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013140533)
store volatile %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$prim49357, align 8
%stackaddr$prim49358 = alloca %struct.ScmObj*, align 8
%args40131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013140533)
store volatile %struct.ScmObj* %args40131, %struct.ScmObj** %stackaddr$prim49358, align 8
%stackaddr$prim49359 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40131)
store volatile %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$prim49359, align 8
%stackaddr$prim49360 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40131)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim49360, align 8
%stackaddr$prim49361 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40278)
store volatile %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$prim49361, align 8
%stackaddr$prim49362 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40131)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim49362, align 8
%stackaddr$prim49363 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40279)
store volatile %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$prim49363, align 8
%stackaddr$makeclosure49364 = alloca %struct.ScmObj*, align 8
%fptrToInt49365 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41305 to i64
%ae41305 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49365)
store volatile %struct.ScmObj* %ae41305, %struct.ScmObj** %stackaddr$makeclosure49364, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41305, %struct.ScmObj* %k40534, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41305, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41305, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41305, %struct.ScmObj* %lsts40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41305, %struct.ScmObj* %_37foldr40130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41305, %struct.ScmObj* %_37foldr140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41305, %struct.ScmObj* %_37map140120, i64 6)
%ae41306 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49366 = alloca %struct.ScmObj*, align 8
%fptrToInt49367 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41307 to i64
%ae41307 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49367)
store volatile %struct.ScmObj* %ae41307, %struct.ScmObj** %stackaddr$makeclosure49366, align 8
%args47690$ae41305$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49368 = alloca %struct.ScmObj*, align 8
%args47690$ae41305$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41307, %struct.ScmObj* %args47690$ae41305$0)
store volatile %struct.ScmObj* %args47690$ae41305$1, %struct.ScmObj** %stackaddr$prim49368, align 8
%stackaddr$prim49369 = alloca %struct.ScmObj*, align 8
%args47690$ae41305$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41306, %struct.ScmObj* %args47690$ae41305$1)
store volatile %struct.ScmObj* %args47690$ae41305$2, %struct.ScmObj** %stackaddr$prim49369, align 8
%clofunc49370 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41305)
musttail call tailcc void %clofunc49370(%struct.ScmObj* %ae41305, %struct.ScmObj* %args47690$ae41305$2)
ret void
}

define tailcc void @proc_clo$ae41305(%struct.ScmObj* %env$ae41305,%struct.ScmObj* %current_45args47636) {
%stackaddr$env-ref49371 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41305, i64 0)
store %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$env-ref49371
%stackaddr$env-ref49372 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41305, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref49372
%stackaddr$env-ref49373 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41305, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref49373
%stackaddr$env-ref49374 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41305, i64 3)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref49374
%stackaddr$env-ref49375 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41305, i64 4)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref49375
%stackaddr$env-ref49376 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41305, i64 5)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49376
%stackaddr$env-ref49377 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41305, i64 6)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref49377
%stackaddr$prim49378 = alloca %struct.ScmObj*, align 8
%_95k40535 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47636)
store volatile %struct.ScmObj* %_95k40535, %struct.ScmObj** %stackaddr$prim49378, align 8
%stackaddr$prim49379 = alloca %struct.ScmObj*, align 8
%current_45args47637 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47636)
store volatile %struct.ScmObj* %current_45args47637, %struct.ScmObj** %stackaddr$prim49379, align 8
%stackaddr$prim49380 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47637)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim49380, align 8
%stackaddr$makeclosure49381 = alloca %struct.ScmObj*, align 8
%fptrToInt49382 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41337 to i64
%ae41337 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49382)
store volatile %struct.ScmObj* %ae41337, %struct.ScmObj** %stackaddr$makeclosure49381, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %k40534, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %lsts40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %_37foldr40130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %_37foldr140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %_37map140120, i64 6)
%ae41339 = call %struct.ScmObj* @const_init_false()
%args47683$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49383 = alloca %struct.ScmObj*, align 8
%args47683$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40132, %struct.ScmObj* %args47683$_37foldr140124$0)
store volatile %struct.ScmObj* %args47683$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim49383, align 8
%stackaddr$prim49384 = alloca %struct.ScmObj*, align 8
%args47683$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41339, %struct.ScmObj* %args47683$_37foldr140124$1)
store volatile %struct.ScmObj* %args47683$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim49384, align 8
%stackaddr$prim49385 = alloca %struct.ScmObj*, align 8
%args47683$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40280, %struct.ScmObj* %args47683$_37foldr140124$2)
store volatile %struct.ScmObj* %args47683$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim49385, align 8
%stackaddr$prim49386 = alloca %struct.ScmObj*, align 8
%args47683$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41337, %struct.ScmObj* %args47683$_37foldr140124$3)
store volatile %struct.ScmObj* %args47683$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim49386, align 8
%clofunc49387 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc49387(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args47683$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae41337(%struct.ScmObj* %env$ae41337,%struct.ScmObj* %current_45args47639) {
%stackaddr$env-ref49388 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 0)
store %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$env-ref49388
%stackaddr$env-ref49389 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref49389
%stackaddr$env-ref49390 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref49390
%stackaddr$env-ref49391 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 3)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref49391
%stackaddr$env-ref49392 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 4)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref49392
%stackaddr$env-ref49393 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 5)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49393
%stackaddr$env-ref49394 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 6)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref49394
%stackaddr$prim49395 = alloca %struct.ScmObj*, align 8
%_95k40536 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47639)
store volatile %struct.ScmObj* %_95k40536, %struct.ScmObj** %stackaddr$prim49395, align 8
%stackaddr$prim49396 = alloca %struct.ScmObj*, align 8
%current_45args47640 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47639)
store volatile %struct.ScmObj* %current_45args47640, %struct.ScmObj** %stackaddr$prim49396, align 8
%stackaddr$prim49397 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47640)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim49397, align 8
%truthy$cmp49398 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40281)
%cmp$cmp49398 = icmp eq i64 %truthy$cmp49398, 1
br i1 %cmp$cmp49398, label %truebranch$cmp49398, label %falsebranch$cmp49398
truebranch$cmp49398:
%ae41348 = call %struct.ScmObj* @const_init_int(i64 0)
%args47642$k40534$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49399 = alloca %struct.ScmObj*, align 8
%args47642$k40534$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40133, %struct.ScmObj* %args47642$k40534$0)
store volatile %struct.ScmObj* %args47642$k40534$1, %struct.ScmObj** %stackaddr$prim49399, align 8
%stackaddr$prim49400 = alloca %struct.ScmObj*, align 8
%args47642$k40534$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41348, %struct.ScmObj* %args47642$k40534$1)
store volatile %struct.ScmObj* %args47642$k40534$2, %struct.ScmObj** %stackaddr$prim49400, align 8
%clofunc49401 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40534)
musttail call tailcc void %clofunc49401(%struct.ScmObj* %k40534, %struct.ScmObj* %args47642$k40534$2)
ret void
falsebranch$cmp49398:
%stackaddr$makeclosure49402 = alloca %struct.ScmObj*, align 8
%fptrToInt49403 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41353 to i64
%ae41353 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49403)
store volatile %struct.ScmObj* %ae41353, %struct.ScmObj** %stackaddr$makeclosure49402, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %k40534, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %lsts40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %_37foldr40130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %_37foldr140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %_37map140120, i64 6)
%ae41354 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49404 = alloca %struct.ScmObj*, align 8
%fptrToInt49405 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41355 to i64
%ae41355 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49405)
store volatile %struct.ScmObj* %ae41355, %struct.ScmObj** %stackaddr$makeclosure49404, align 8
%args47682$ae41353$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49406 = alloca %struct.ScmObj*, align 8
%args47682$ae41353$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41355, %struct.ScmObj* %args47682$ae41353$0)
store volatile %struct.ScmObj* %args47682$ae41353$1, %struct.ScmObj** %stackaddr$prim49406, align 8
%stackaddr$prim49407 = alloca %struct.ScmObj*, align 8
%args47682$ae41353$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41354, %struct.ScmObj* %args47682$ae41353$1)
store volatile %struct.ScmObj* %args47682$ae41353$2, %struct.ScmObj** %stackaddr$prim49407, align 8
%clofunc49408 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41353)
musttail call tailcc void %clofunc49408(%struct.ScmObj* %ae41353, %struct.ScmObj* %args47682$ae41353$2)
ret void
}

define tailcc void @proc_clo$ae41353(%struct.ScmObj* %env$ae41353,%struct.ScmObj* %current_45args47643) {
%stackaddr$env-ref49409 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 0)
store %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$env-ref49409
%stackaddr$env-ref49410 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref49410
%stackaddr$env-ref49411 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref49411
%stackaddr$env-ref49412 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 3)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref49412
%stackaddr$env-ref49413 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 4)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref49413
%stackaddr$env-ref49414 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 5)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49414
%stackaddr$env-ref49415 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 6)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref49415
%stackaddr$prim49416 = alloca %struct.ScmObj*, align 8
%_95k40537 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47643)
store volatile %struct.ScmObj* %_95k40537, %struct.ScmObj** %stackaddr$prim49416, align 8
%stackaddr$prim49417 = alloca %struct.ScmObj*, align 8
%current_45args47644 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47643)
store volatile %struct.ScmObj* %current_45args47644, %struct.ScmObj** %stackaddr$prim49417, align 8
%stackaddr$prim49418 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47644)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim49418, align 8
%stackaddr$makeclosure49419 = alloca %struct.ScmObj*, align 8
%fptrToInt49420 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41374 to i64
%ae41374 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49420)
store volatile %struct.ScmObj* %ae41374, %struct.ScmObj** %stackaddr$makeclosure49419, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %k40534, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %lsts40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %_37foldr40130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %_37foldr140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %_37map140120, i64 6)
%args47677$_37map140120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49421 = alloca %struct.ScmObj*, align 8
%args47677$_37map140120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40132, %struct.ScmObj* %args47677$_37map140120$0)
store volatile %struct.ScmObj* %args47677$_37map140120$1, %struct.ScmObj** %stackaddr$prim49421, align 8
%stackaddr$prim49422 = alloca %struct.ScmObj*, align 8
%args47677$_37map140120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40282, %struct.ScmObj* %args47677$_37map140120$1)
store volatile %struct.ScmObj* %args47677$_37map140120$2, %struct.ScmObj** %stackaddr$prim49422, align 8
%stackaddr$prim49423 = alloca %struct.ScmObj*, align 8
%args47677$_37map140120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41374, %struct.ScmObj* %args47677$_37map140120$2)
store volatile %struct.ScmObj* %args47677$_37map140120$3, %struct.ScmObj** %stackaddr$prim49423, align 8
%clofunc49424 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140120)
musttail call tailcc void %clofunc49424(%struct.ScmObj* %_37map140120, %struct.ScmObj* %args47677$_37map140120$3)
ret void
}

define tailcc void @proc_clo$ae41374(%struct.ScmObj* %env$ae41374,%struct.ScmObj* %current_45args47646) {
%stackaddr$env-ref49425 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 0)
store %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$env-ref49425
%stackaddr$env-ref49426 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref49426
%stackaddr$env-ref49427 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref49427
%stackaddr$env-ref49428 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 3)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref49428
%stackaddr$env-ref49429 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 4)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref49429
%stackaddr$env-ref49430 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 5)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49430
%stackaddr$env-ref49431 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 6)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref49431
%stackaddr$prim49432 = alloca %struct.ScmObj*, align 8
%_95k40538 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47646)
store volatile %struct.ScmObj* %_95k40538, %struct.ScmObj** %stackaddr$prim49432, align 8
%stackaddr$prim49433 = alloca %struct.ScmObj*, align 8
%current_45args47647 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47646)
store volatile %struct.ScmObj* %current_45args47647, %struct.ScmObj** %stackaddr$prim49433, align 8
%stackaddr$prim49434 = alloca %struct.ScmObj*, align 8
%lsts_4340139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47647)
store volatile %struct.ScmObj* %lsts_4340139, %struct.ScmObj** %stackaddr$prim49434, align 8
%stackaddr$makeclosure49435 = alloca %struct.ScmObj*, align 8
%fptrToInt49436 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41377 to i64
%ae41377 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt49436)
store volatile %struct.ScmObj* %ae41377, %struct.ScmObj** %stackaddr$makeclosure49435, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %k40534, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %lsts40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %_37foldr40130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %_37foldr140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %lsts_4340139, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %_37map140120, i64 7)
%ae41378 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49437 = alloca %struct.ScmObj*, align 8
%fptrToInt49438 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41379 to i64
%ae41379 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49438)
store volatile %struct.ScmObj* %ae41379, %struct.ScmObj** %stackaddr$makeclosure49437, align 8
%args47676$ae41377$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49439 = alloca %struct.ScmObj*, align 8
%args47676$ae41377$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41379, %struct.ScmObj* %args47676$ae41377$0)
store volatile %struct.ScmObj* %args47676$ae41377$1, %struct.ScmObj** %stackaddr$prim49439, align 8
%stackaddr$prim49440 = alloca %struct.ScmObj*, align 8
%args47676$ae41377$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41378, %struct.ScmObj* %args47676$ae41377$1)
store volatile %struct.ScmObj* %args47676$ae41377$2, %struct.ScmObj** %stackaddr$prim49440, align 8
%clofunc49441 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41377)
musttail call tailcc void %clofunc49441(%struct.ScmObj* %ae41377, %struct.ScmObj* %args47676$ae41377$2)
ret void
}

define tailcc void @proc_clo$ae41377(%struct.ScmObj* %env$ae41377,%struct.ScmObj* %current_45args47649) {
%stackaddr$env-ref49442 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 0)
store %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$env-ref49442
%stackaddr$env-ref49443 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref49443
%stackaddr$env-ref49444 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref49444
%stackaddr$env-ref49445 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 3)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref49445
%stackaddr$env-ref49446 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 4)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref49446
%stackaddr$env-ref49447 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 5)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49447
%stackaddr$env-ref49448 = alloca %struct.ScmObj*, align 8
%lsts_4340139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 6)
store %struct.ScmObj* %lsts_4340139, %struct.ScmObj** %stackaddr$env-ref49448
%stackaddr$env-ref49449 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 7)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref49449
%stackaddr$prim49450 = alloca %struct.ScmObj*, align 8
%_95k40539 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47649)
store volatile %struct.ScmObj* %_95k40539, %struct.ScmObj** %stackaddr$prim49450, align 8
%stackaddr$prim49451 = alloca %struct.ScmObj*, align 8
%current_45args47650 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47649)
store volatile %struct.ScmObj* %current_45args47650, %struct.ScmObj** %stackaddr$prim49451, align 8
%stackaddr$prim49452 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47650)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim49452, align 8
%stackaddr$makeclosure49453 = alloca %struct.ScmObj*, align 8
%fptrToInt49454 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41398 to i64
%ae41398 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49454)
store volatile %struct.ScmObj* %ae41398, %struct.ScmObj** %stackaddr$makeclosure49453, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41398, %struct.ScmObj* %k40534, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41398, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41398, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41398, %struct.ScmObj* %_37foldr40130, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41398, %struct.ScmObj* %_37foldr140124, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41398, %struct.ScmObj* %lsts_4340139, i64 5)
%args47671$_37map140120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49455 = alloca %struct.ScmObj*, align 8
%args47671$_37map140120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40132, %struct.ScmObj* %args47671$_37map140120$0)
store volatile %struct.ScmObj* %args47671$_37map140120$1, %struct.ScmObj** %stackaddr$prim49455, align 8
%stackaddr$prim49456 = alloca %struct.ScmObj*, align 8
%args47671$_37map140120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40283, %struct.ScmObj* %args47671$_37map140120$1)
store volatile %struct.ScmObj* %args47671$_37map140120$2, %struct.ScmObj** %stackaddr$prim49456, align 8
%stackaddr$prim49457 = alloca %struct.ScmObj*, align 8
%args47671$_37map140120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41398, %struct.ScmObj* %args47671$_37map140120$2)
store volatile %struct.ScmObj* %args47671$_37map140120$3, %struct.ScmObj** %stackaddr$prim49457, align 8
%clofunc49458 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140120)
musttail call tailcc void %clofunc49458(%struct.ScmObj* %_37map140120, %struct.ScmObj* %args47671$_37map140120$3)
ret void
}

define tailcc void @proc_clo$ae41398(%struct.ScmObj* %env$ae41398,%struct.ScmObj* %current_45args47652) {
%stackaddr$env-ref49459 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41398, i64 0)
store %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$env-ref49459
%stackaddr$env-ref49460 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41398, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref49460
%stackaddr$env-ref49461 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41398, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref49461
%stackaddr$env-ref49462 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41398, i64 3)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref49462
%stackaddr$env-ref49463 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41398, i64 4)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49463
%stackaddr$env-ref49464 = alloca %struct.ScmObj*, align 8
%lsts_4340139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41398, i64 5)
store %struct.ScmObj* %lsts_4340139, %struct.ScmObj** %stackaddr$env-ref49464
%stackaddr$prim49465 = alloca %struct.ScmObj*, align 8
%_95k40540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47652)
store volatile %struct.ScmObj* %_95k40540, %struct.ScmObj** %stackaddr$prim49465, align 8
%stackaddr$prim49466 = alloca %struct.ScmObj*, align 8
%current_45args47653 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47652)
store volatile %struct.ScmObj* %current_45args47653, %struct.ScmObj** %stackaddr$prim49466, align 8
%stackaddr$prim49467 = alloca %struct.ScmObj*, align 8
%vs40137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47653)
store volatile %struct.ScmObj* %vs40137, %struct.ScmObj** %stackaddr$prim49467, align 8
%stackaddr$makeclosure49468 = alloca %struct.ScmObj*, align 8
%fptrToInt49469 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41401 to i64
%ae41401 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49469)
store volatile %struct.ScmObj* %ae41401, %struct.ScmObj** %stackaddr$makeclosure49468, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41401, %struct.ScmObj* %k40534, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41401, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41401, %struct.ScmObj* %acc40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41401, %struct.ScmObj* %_37foldr40130, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41401, %struct.ScmObj* %_37foldr140124, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41401, %struct.ScmObj* %lsts_4340139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41401, %struct.ScmObj* %vs40137, i64 6)
%ae41402 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49470 = alloca %struct.ScmObj*, align 8
%fptrToInt49471 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41403 to i64
%ae41403 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49471)
store volatile %struct.ScmObj* %ae41403, %struct.ScmObj** %stackaddr$makeclosure49470, align 8
%args47670$ae41401$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49472 = alloca %struct.ScmObj*, align 8
%args47670$ae41401$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41403, %struct.ScmObj* %args47670$ae41401$0)
store volatile %struct.ScmObj* %args47670$ae41401$1, %struct.ScmObj** %stackaddr$prim49472, align 8
%stackaddr$prim49473 = alloca %struct.ScmObj*, align 8
%args47670$ae41401$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41402, %struct.ScmObj* %args47670$ae41401$1)
store volatile %struct.ScmObj* %args47670$ae41401$2, %struct.ScmObj** %stackaddr$prim49473, align 8
%clofunc49474 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41401)
musttail call tailcc void %clofunc49474(%struct.ScmObj* %ae41401, %struct.ScmObj* %args47670$ae41401$2)
ret void
}

define tailcc void @proc_clo$ae41401(%struct.ScmObj* %env$ae41401,%struct.ScmObj* %current_45args47655) {
%stackaddr$env-ref49475 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41401, i64 0)
store %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$env-ref49475
%stackaddr$env-ref49476 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41401, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref49476
%stackaddr$env-ref49477 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41401, i64 2)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref49477
%stackaddr$env-ref49478 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41401, i64 3)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref49478
%stackaddr$env-ref49479 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41401, i64 4)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49479
%stackaddr$env-ref49480 = alloca %struct.ScmObj*, align 8
%lsts_4340139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41401, i64 5)
store %struct.ScmObj* %lsts_4340139, %struct.ScmObj** %stackaddr$env-ref49480
%stackaddr$env-ref49481 = alloca %struct.ScmObj*, align 8
%vs40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41401, i64 6)
store %struct.ScmObj* %vs40137, %struct.ScmObj** %stackaddr$env-ref49481
%stackaddr$prim49482 = alloca %struct.ScmObj*, align 8
%_95k40541 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47655)
store volatile %struct.ScmObj* %_95k40541, %struct.ScmObj** %stackaddr$prim49482, align 8
%stackaddr$prim49483 = alloca %struct.ScmObj*, align 8
%current_45args47656 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47655)
store volatile %struct.ScmObj* %current_45args47656, %struct.ScmObj** %stackaddr$prim49483, align 8
%stackaddr$prim49484 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47656)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim49484, align 8
%stackaddr$prim49485 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40133, %struct.ScmObj* %lsts_4340139)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim49485, align 8
%stackaddr$prim49486 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40134, %struct.ScmObj* %anf_45bind40285)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim49486, align 8
%stackaddr$makeclosure49487 = alloca %struct.ScmObj*, align 8
%fptrToInt49488 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41427 to i64
%ae41427 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49488)
store volatile %struct.ScmObj* %ae41427, %struct.ScmObj** %stackaddr$makeclosure49487, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41427, %struct.ScmObj* %k40534, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41427, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41427, %struct.ScmObj* %anf_45bind40284, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41427, %struct.ScmObj* %_37foldr140124, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41427, %struct.ScmObj* %vs40137, i64 4)
%stackaddr$prim49489 = alloca %struct.ScmObj*, align 8
%cpsargs40545 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41427, %struct.ScmObj* %anf_45bind40286)
store volatile %struct.ScmObj* %cpsargs40545, %struct.ScmObj** %stackaddr$prim49489, align 8
%clofunc49490 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40130)
musttail call tailcc void %clofunc49490(%struct.ScmObj* %_37foldr40130, %struct.ScmObj* %cpsargs40545)
ret void
}

define tailcc void @proc_clo$ae41427(%struct.ScmObj* %env$ae41427,%struct.ScmObj* %current_45args47658) {
%stackaddr$env-ref49491 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41427, i64 0)
store %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$env-ref49491
%stackaddr$env-ref49492 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41427, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref49492
%stackaddr$env-ref49493 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41427, i64 2)
store %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$env-ref49493
%stackaddr$env-ref49494 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41427, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49494
%stackaddr$env-ref49495 = alloca %struct.ScmObj*, align 8
%vs40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41427, i64 4)
store %struct.ScmObj* %vs40137, %struct.ScmObj** %stackaddr$env-ref49495
%stackaddr$prim49496 = alloca %struct.ScmObj*, align 8
%_95k40542 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47658)
store volatile %struct.ScmObj* %_95k40542, %struct.ScmObj** %stackaddr$prim49496, align 8
%stackaddr$prim49497 = alloca %struct.ScmObj*, align 8
%current_45args47659 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47658)
store volatile %struct.ScmObj* %current_45args47659, %struct.ScmObj** %stackaddr$prim49497, align 8
%stackaddr$prim49498 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47659)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim49498, align 8
%ae41432 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49499 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40287, %struct.ScmObj* %ae41432)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim49499, align 8
%stackaddr$makeclosure49500 = alloca %struct.ScmObj*, align 8
%fptrToInt49501 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41434 to i64
%ae41434 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49501)
store volatile %struct.ScmObj* %ae41434, %struct.ScmObj** %stackaddr$makeclosure49500, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41434, %struct.ScmObj* %k40534, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41434, %struct.ScmObj* %f40134, i64 1)
%args47664$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49502 = alloca %struct.ScmObj*, align 8
%args47664$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40137, %struct.ScmObj* %args47664$_37foldr140124$0)
store volatile %struct.ScmObj* %args47664$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim49502, align 8
%stackaddr$prim49503 = alloca %struct.ScmObj*, align 8
%args47664$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40288, %struct.ScmObj* %args47664$_37foldr140124$1)
store volatile %struct.ScmObj* %args47664$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim49503, align 8
%stackaddr$prim49504 = alloca %struct.ScmObj*, align 8
%args47664$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40284, %struct.ScmObj* %args47664$_37foldr140124$2)
store volatile %struct.ScmObj* %args47664$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim49504, align 8
%stackaddr$prim49505 = alloca %struct.ScmObj*, align 8
%args47664$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41434, %struct.ScmObj* %args47664$_37foldr140124$3)
store volatile %struct.ScmObj* %args47664$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim49505, align 8
%clofunc49506 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc49506(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args47664$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae41434(%struct.ScmObj* %env$ae41434,%struct.ScmObj* %current_45args47661) {
%stackaddr$env-ref49507 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41434, i64 0)
store %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$env-ref49507
%stackaddr$env-ref49508 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41434, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref49508
%stackaddr$prim49509 = alloca %struct.ScmObj*, align 8
%_95k40543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47661)
store volatile %struct.ScmObj* %_95k40543, %struct.ScmObj** %stackaddr$prim49509, align 8
%stackaddr$prim49510 = alloca %struct.ScmObj*, align 8
%current_45args47662 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47661)
store volatile %struct.ScmObj* %current_45args47662, %struct.ScmObj** %stackaddr$prim49510, align 8
%stackaddr$prim49511 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47662)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim49511, align 8
%stackaddr$prim49512 = alloca %struct.ScmObj*, align 8
%cpsargs40544 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40534, %struct.ScmObj* %anf_45bind40289)
store volatile %struct.ScmObj* %cpsargs40544, %struct.ScmObj** %stackaddr$prim49512, align 8
%clofunc49513 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40134)
musttail call tailcc void %clofunc49513(%struct.ScmObj* %f40134, %struct.ScmObj* %cpsargs40544)
ret void
}

define tailcc void @proc_clo$ae41403(%struct.ScmObj* %env$ae41403,%struct.ScmObj* %current_45args47665) {
%stackaddr$prim49514 = alloca %struct.ScmObj*, align 8
%k40546 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47665)
store volatile %struct.ScmObj* %k40546, %struct.ScmObj** %stackaddr$prim49514, align 8
%stackaddr$prim49515 = alloca %struct.ScmObj*, align 8
%current_45args47666 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47665)
store volatile %struct.ScmObj* %current_45args47666, %struct.ScmObj** %stackaddr$prim49515, align 8
%stackaddr$prim49516 = alloca %struct.ScmObj*, align 8
%a40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47666)
store volatile %struct.ScmObj* %a40142, %struct.ScmObj** %stackaddr$prim49516, align 8
%stackaddr$prim49517 = alloca %struct.ScmObj*, align 8
%current_45args47667 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47666)
store volatile %struct.ScmObj* %current_45args47667, %struct.ScmObj** %stackaddr$prim49517, align 8
%stackaddr$prim49518 = alloca %struct.ScmObj*, align 8
%b40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47667)
store volatile %struct.ScmObj* %b40141, %struct.ScmObj** %stackaddr$prim49518, align 8
%stackaddr$prim49519 = alloca %struct.ScmObj*, align 8
%cpsprim40547 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40142, %struct.ScmObj* %b40141)
store volatile %struct.ScmObj* %cpsprim40547, %struct.ScmObj** %stackaddr$prim49519, align 8
%ae41407 = call %struct.ScmObj* @const_init_int(i64 0)
%args47669$k40546$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49520 = alloca %struct.ScmObj*, align 8
%args47669$k40546$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40547, %struct.ScmObj* %args47669$k40546$0)
store volatile %struct.ScmObj* %args47669$k40546$1, %struct.ScmObj** %stackaddr$prim49520, align 8
%stackaddr$prim49521 = alloca %struct.ScmObj*, align 8
%args47669$k40546$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41407, %struct.ScmObj* %args47669$k40546$1)
store volatile %struct.ScmObj* %args47669$k40546$2, %struct.ScmObj** %stackaddr$prim49521, align 8
%clofunc49522 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40546)
musttail call tailcc void %clofunc49522(%struct.ScmObj* %k40546, %struct.ScmObj* %args47669$k40546$2)
ret void
}

define tailcc void @proc_clo$ae41379(%struct.ScmObj* %env$ae41379,%struct.ScmObj* %current_45args47672) {
%stackaddr$prim49523 = alloca %struct.ScmObj*, align 8
%k40548 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47672)
store volatile %struct.ScmObj* %k40548, %struct.ScmObj** %stackaddr$prim49523, align 8
%stackaddr$prim49524 = alloca %struct.ScmObj*, align 8
%current_45args47673 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47672)
store volatile %struct.ScmObj* %current_45args47673, %struct.ScmObj** %stackaddr$prim49524, align 8
%stackaddr$prim49525 = alloca %struct.ScmObj*, align 8
%x40138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47673)
store volatile %struct.ScmObj* %x40138, %struct.ScmObj** %stackaddr$prim49525, align 8
%stackaddr$prim49526 = alloca %struct.ScmObj*, align 8
%cpsprim40549 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40138)
store volatile %struct.ScmObj* %cpsprim40549, %struct.ScmObj** %stackaddr$prim49526, align 8
%ae41382 = call %struct.ScmObj* @const_init_int(i64 0)
%args47675$k40548$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49527 = alloca %struct.ScmObj*, align 8
%args47675$k40548$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40549, %struct.ScmObj* %args47675$k40548$0)
store volatile %struct.ScmObj* %args47675$k40548$1, %struct.ScmObj** %stackaddr$prim49527, align 8
%stackaddr$prim49528 = alloca %struct.ScmObj*, align 8
%args47675$k40548$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41382, %struct.ScmObj* %args47675$k40548$1)
store volatile %struct.ScmObj* %args47675$k40548$2, %struct.ScmObj** %stackaddr$prim49528, align 8
%clofunc49529 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40548)
musttail call tailcc void %clofunc49529(%struct.ScmObj* %k40548, %struct.ScmObj* %args47675$k40548$2)
ret void
}

define tailcc void @proc_clo$ae41355(%struct.ScmObj* %env$ae41355,%struct.ScmObj* %current_45args47678) {
%stackaddr$prim49530 = alloca %struct.ScmObj*, align 8
%k40550 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47678)
store volatile %struct.ScmObj* %k40550, %struct.ScmObj** %stackaddr$prim49530, align 8
%stackaddr$prim49531 = alloca %struct.ScmObj*, align 8
%current_45args47679 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47678)
store volatile %struct.ScmObj* %current_45args47679, %struct.ScmObj** %stackaddr$prim49531, align 8
%stackaddr$prim49532 = alloca %struct.ScmObj*, align 8
%x40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47679)
store volatile %struct.ScmObj* %x40140, %struct.ScmObj** %stackaddr$prim49532, align 8
%stackaddr$prim49533 = alloca %struct.ScmObj*, align 8
%cpsprim40551 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40140)
store volatile %struct.ScmObj* %cpsprim40551, %struct.ScmObj** %stackaddr$prim49533, align 8
%ae41358 = call %struct.ScmObj* @const_init_int(i64 0)
%args47681$k40550$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49534 = alloca %struct.ScmObj*, align 8
%args47681$k40550$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40551, %struct.ScmObj* %args47681$k40550$0)
store volatile %struct.ScmObj* %args47681$k40550$1, %struct.ScmObj** %stackaddr$prim49534, align 8
%stackaddr$prim49535 = alloca %struct.ScmObj*, align 8
%args47681$k40550$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41358, %struct.ScmObj* %args47681$k40550$1)
store volatile %struct.ScmObj* %args47681$k40550$2, %struct.ScmObj** %stackaddr$prim49535, align 8
%clofunc49536 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40550)
musttail call tailcc void %clofunc49536(%struct.ScmObj* %k40550, %struct.ScmObj* %args47681$k40550$2)
ret void
}

define tailcc void @proc_clo$ae41307(%struct.ScmObj* %env$ae41307,%struct.ScmObj* %current_45args47684) {
%stackaddr$prim49537 = alloca %struct.ScmObj*, align 8
%k40552 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47684)
store volatile %struct.ScmObj* %k40552, %struct.ScmObj** %stackaddr$prim49537, align 8
%stackaddr$prim49538 = alloca %struct.ScmObj*, align 8
%current_45args47685 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47684)
store volatile %struct.ScmObj* %current_45args47685, %struct.ScmObj** %stackaddr$prim49538, align 8
%stackaddr$prim49539 = alloca %struct.ScmObj*, align 8
%lst40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47685)
store volatile %struct.ScmObj* %lst40136, %struct.ScmObj** %stackaddr$prim49539, align 8
%stackaddr$prim49540 = alloca %struct.ScmObj*, align 8
%current_45args47686 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47685)
store volatile %struct.ScmObj* %current_45args47686, %struct.ScmObj** %stackaddr$prim49540, align 8
%stackaddr$prim49541 = alloca %struct.ScmObj*, align 8
%b40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47686)
store volatile %struct.ScmObj* %b40135, %struct.ScmObj** %stackaddr$prim49541, align 8
%truthy$cmp49542 = call i64 @is_truthy_value(%struct.ScmObj* %b40135)
%cmp$cmp49542 = icmp eq i64 %truthy$cmp49542, 1
br i1 %cmp$cmp49542, label %truebranch$cmp49542, label %falsebranch$cmp49542
truebranch$cmp49542:
%ae41310 = call %struct.ScmObj* @const_init_int(i64 0)
%args47688$k40552$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49543 = alloca %struct.ScmObj*, align 8
%args47688$k40552$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40135, %struct.ScmObj* %args47688$k40552$0)
store volatile %struct.ScmObj* %args47688$k40552$1, %struct.ScmObj** %stackaddr$prim49543, align 8
%stackaddr$prim49544 = alloca %struct.ScmObj*, align 8
%args47688$k40552$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41310, %struct.ScmObj* %args47688$k40552$1)
store volatile %struct.ScmObj* %args47688$k40552$2, %struct.ScmObj** %stackaddr$prim49544, align 8
%clofunc49545 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40552)
musttail call tailcc void %clofunc49545(%struct.ScmObj* %k40552, %struct.ScmObj* %args47688$k40552$2)
ret void
falsebranch$cmp49542:
%stackaddr$prim49546 = alloca %struct.ScmObj*, align 8
%cpsprim40553 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40136)
store volatile %struct.ScmObj* %cpsprim40553, %struct.ScmObj** %stackaddr$prim49546, align 8
%ae41317 = call %struct.ScmObj* @const_init_int(i64 0)
%args47689$k40552$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49547 = alloca %struct.ScmObj*, align 8
%args47689$k40552$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40553, %struct.ScmObj* %args47689$k40552$0)
store volatile %struct.ScmObj* %args47689$k40552$1, %struct.ScmObj** %stackaddr$prim49547, align 8
%stackaddr$prim49548 = alloca %struct.ScmObj*, align 8
%args47689$k40552$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41317, %struct.ScmObj* %args47689$k40552$1)
store volatile %struct.ScmObj* %args47689$k40552$2, %struct.ScmObj** %stackaddr$prim49548, align 8
%clofunc49549 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40552)
musttail call tailcc void %clofunc49549(%struct.ScmObj* %k40552, %struct.ScmObj* %args47689$k40552$2)
ret void
}

define tailcc void @proc_clo$ae41264(%struct.ScmObj* %env$ae41264,%struct.ScmObj* %current_45args47693) {
%stackaddr$env-ref49550 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41264, i64 0)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref49550
%stackaddr$env-ref49551 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41264, i64 1)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref49551
%stackaddr$prim49552 = alloca %struct.ScmObj*, align 8
%k40554 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47693)
store volatile %struct.ScmObj* %k40554, %struct.ScmObj** %stackaddr$prim49552, align 8
%stackaddr$prim49553 = alloca %struct.ScmObj*, align 8
%current_45args47694 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47693)
store volatile %struct.ScmObj* %current_45args47694, %struct.ScmObj** %stackaddr$prim49553, align 8
%stackaddr$prim49554 = alloca %struct.ScmObj*, align 8
%lst40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47694)
store volatile %struct.ScmObj* %lst40145, %struct.ScmObj** %stackaddr$prim49554, align 8
%stackaddr$prim49555 = alloca %struct.ScmObj*, align 8
%current_45args47695 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47694)
store volatile %struct.ScmObj* %current_45args47695, %struct.ScmObj** %stackaddr$prim49555, align 8
%stackaddr$prim49556 = alloca %struct.ScmObj*, align 8
%n40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47695)
store volatile %struct.ScmObj* %n40144, %struct.ScmObj** %stackaddr$prim49556, align 8
%stackaddr$makeclosure49557 = alloca %struct.ScmObj*, align 8
%fptrToInt49558 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41266 to i64
%ae41266 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49558)
store volatile %struct.ScmObj* %ae41266, %struct.ScmObj** %stackaddr$makeclosure49557, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41266, %struct.ScmObj* %_37take40116, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41266, %struct.ScmObj* %lst40145, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41266, %struct.ScmObj* %n40144, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41266, %struct.ScmObj* %k40554, i64 3)
%args47701$_37length40113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49559 = alloca %struct.ScmObj*, align 8
%args47701$_37length40113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40145, %struct.ScmObj* %args47701$_37length40113$0)
store volatile %struct.ScmObj* %args47701$_37length40113$1, %struct.ScmObj** %stackaddr$prim49559, align 8
%stackaddr$prim49560 = alloca %struct.ScmObj*, align 8
%args47701$_37length40113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41266, %struct.ScmObj* %args47701$_37length40113$1)
store volatile %struct.ScmObj* %args47701$_37length40113$2, %struct.ScmObj** %stackaddr$prim49560, align 8
%clofunc49561 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40113)
musttail call tailcc void %clofunc49561(%struct.ScmObj* %_37length40113, %struct.ScmObj* %args47701$_37length40113$2)
ret void
}

define tailcc void @proc_clo$ae41266(%struct.ScmObj* %env$ae41266,%struct.ScmObj* %current_45args47697) {
%stackaddr$env-ref49562 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41266, i64 0)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref49562
%stackaddr$env-ref49563 = alloca %struct.ScmObj*, align 8
%lst40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41266, i64 1)
store %struct.ScmObj* %lst40145, %struct.ScmObj** %stackaddr$env-ref49563
%stackaddr$env-ref49564 = alloca %struct.ScmObj*, align 8
%n40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41266, i64 2)
store %struct.ScmObj* %n40144, %struct.ScmObj** %stackaddr$env-ref49564
%stackaddr$env-ref49565 = alloca %struct.ScmObj*, align 8
%k40554 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41266, i64 3)
store %struct.ScmObj* %k40554, %struct.ScmObj** %stackaddr$env-ref49565
%stackaddr$prim49566 = alloca %struct.ScmObj*, align 8
%_95k40555 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47697)
store volatile %struct.ScmObj* %_95k40555, %struct.ScmObj** %stackaddr$prim49566, align 8
%stackaddr$prim49567 = alloca %struct.ScmObj*, align 8
%current_45args47698 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47697)
store volatile %struct.ScmObj* %current_45args47698, %struct.ScmObj** %stackaddr$prim49567, align 8
%stackaddr$prim49568 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47698)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim49568, align 8
%stackaddr$prim49569 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40276, %struct.ScmObj* %n40144)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim49569, align 8
%args47700$_37take40116$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49570 = alloca %struct.ScmObj*, align 8
%args47700$_37take40116$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40277, %struct.ScmObj* %args47700$_37take40116$0)
store volatile %struct.ScmObj* %args47700$_37take40116$1, %struct.ScmObj** %stackaddr$prim49570, align 8
%stackaddr$prim49571 = alloca %struct.ScmObj*, align 8
%args47700$_37take40116$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40145, %struct.ScmObj* %args47700$_37take40116$1)
store volatile %struct.ScmObj* %args47700$_37take40116$2, %struct.ScmObj** %stackaddr$prim49571, align 8
%stackaddr$prim49572 = alloca %struct.ScmObj*, align 8
%args47700$_37take40116$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40554, %struct.ScmObj* %args47700$_37take40116$2)
store volatile %struct.ScmObj* %args47700$_37take40116$3, %struct.ScmObj** %stackaddr$prim49572, align 8
%clofunc49573 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40116)
musttail call tailcc void %clofunc49573(%struct.ScmObj* %_37take40116, %struct.ScmObj* %args47700$_37take40116$3)
ret void
}

define tailcc void @proc_clo$ae41210(%struct.ScmObj* %env$ae41210,%struct.ScmObj* %current_45args47703) {
%stackaddr$env-ref49574 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41210, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref49574
%stackaddr$prim49575 = alloca %struct.ScmObj*, align 8
%k40556 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47703)
store volatile %struct.ScmObj* %k40556, %struct.ScmObj** %stackaddr$prim49575, align 8
%stackaddr$prim49576 = alloca %struct.ScmObj*, align 8
%current_45args47704 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47703)
store volatile %struct.ScmObj* %current_45args47704, %struct.ScmObj** %stackaddr$prim49576, align 8
%stackaddr$prim49577 = alloca %struct.ScmObj*, align 8
%lst40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47704)
store volatile %struct.ScmObj* %lst40147, %struct.ScmObj** %stackaddr$prim49577, align 8
%stackaddr$makeclosure49578 = alloca %struct.ScmObj*, align 8
%fptrToInt49579 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41211 to i64
%ae41211 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49579)
store volatile %struct.ScmObj* %ae41211, %struct.ScmObj** %stackaddr$makeclosure49578, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41211, %struct.ScmObj* %k40556, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41211, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41211, %struct.ScmObj* %lst40147, i64 2)
%ae41212 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49580 = alloca %struct.ScmObj*, align 8
%fptrToInt49581 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41213 to i64
%ae41213 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49581)
store volatile %struct.ScmObj* %ae41213, %struct.ScmObj** %stackaddr$makeclosure49580, align 8
%args47715$ae41211$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49582 = alloca %struct.ScmObj*, align 8
%args47715$ae41211$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41213, %struct.ScmObj* %args47715$ae41211$0)
store volatile %struct.ScmObj* %args47715$ae41211$1, %struct.ScmObj** %stackaddr$prim49582, align 8
%stackaddr$prim49583 = alloca %struct.ScmObj*, align 8
%args47715$ae41211$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41212, %struct.ScmObj* %args47715$ae41211$1)
store volatile %struct.ScmObj* %args47715$ae41211$2, %struct.ScmObj** %stackaddr$prim49583, align 8
%clofunc49584 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41211)
musttail call tailcc void %clofunc49584(%struct.ScmObj* %ae41211, %struct.ScmObj* %args47715$ae41211$2)
ret void
}

define tailcc void @proc_clo$ae41211(%struct.ScmObj* %env$ae41211,%struct.ScmObj* %current_45args47706) {
%stackaddr$env-ref49585 = alloca %struct.ScmObj*, align 8
%k40556 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41211, i64 0)
store %struct.ScmObj* %k40556, %struct.ScmObj** %stackaddr$env-ref49585
%stackaddr$env-ref49586 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41211, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref49586
%stackaddr$env-ref49587 = alloca %struct.ScmObj*, align 8
%lst40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41211, i64 2)
store %struct.ScmObj* %lst40147, %struct.ScmObj** %stackaddr$env-ref49587
%stackaddr$prim49588 = alloca %struct.ScmObj*, align 8
%_95k40557 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47706)
store volatile %struct.ScmObj* %_95k40557, %struct.ScmObj** %stackaddr$prim49588, align 8
%stackaddr$prim49589 = alloca %struct.ScmObj*, align 8
%current_45args47707 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47706)
store volatile %struct.ScmObj* %current_45args47707, %struct.ScmObj** %stackaddr$prim49589, align 8
%stackaddr$prim49590 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47707)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim49590, align 8
%ae41232 = call %struct.ScmObj* @const_init_null()
%args47709$_37foldl140108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49591 = alloca %struct.ScmObj*, align 8
%args47709$_37foldl140108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40147, %struct.ScmObj* %args47709$_37foldl140108$0)
store volatile %struct.ScmObj* %args47709$_37foldl140108$1, %struct.ScmObj** %stackaddr$prim49591, align 8
%stackaddr$prim49592 = alloca %struct.ScmObj*, align 8
%args47709$_37foldl140108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41232, %struct.ScmObj* %args47709$_37foldl140108$1)
store volatile %struct.ScmObj* %args47709$_37foldl140108$2, %struct.ScmObj** %stackaddr$prim49592, align 8
%stackaddr$prim49593 = alloca %struct.ScmObj*, align 8
%args47709$_37foldl140108$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40275, %struct.ScmObj* %args47709$_37foldl140108$2)
store volatile %struct.ScmObj* %args47709$_37foldl140108$3, %struct.ScmObj** %stackaddr$prim49593, align 8
%stackaddr$prim49594 = alloca %struct.ScmObj*, align 8
%args47709$_37foldl140108$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40556, %struct.ScmObj* %args47709$_37foldl140108$3)
store volatile %struct.ScmObj* %args47709$_37foldl140108$4, %struct.ScmObj** %stackaddr$prim49594, align 8
%clofunc49595 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140108)
musttail call tailcc void %clofunc49595(%struct.ScmObj* %_37foldl140108, %struct.ScmObj* %args47709$_37foldl140108$4)
ret void
}

define tailcc void @proc_clo$ae41213(%struct.ScmObj* %env$ae41213,%struct.ScmObj* %current_45args47710) {
%stackaddr$prim49596 = alloca %struct.ScmObj*, align 8
%k40558 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47710)
store volatile %struct.ScmObj* %k40558, %struct.ScmObj** %stackaddr$prim49596, align 8
%stackaddr$prim49597 = alloca %struct.ScmObj*, align 8
%current_45args47711 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47710)
store volatile %struct.ScmObj* %current_45args47711, %struct.ScmObj** %stackaddr$prim49597, align 8
%stackaddr$prim49598 = alloca %struct.ScmObj*, align 8
%x40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47711)
store volatile %struct.ScmObj* %x40149, %struct.ScmObj** %stackaddr$prim49598, align 8
%stackaddr$prim49599 = alloca %struct.ScmObj*, align 8
%current_45args47712 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47711)
store volatile %struct.ScmObj* %current_45args47712, %struct.ScmObj** %stackaddr$prim49599, align 8
%stackaddr$prim49600 = alloca %struct.ScmObj*, align 8
%y40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47712)
store volatile %struct.ScmObj* %y40148, %struct.ScmObj** %stackaddr$prim49600, align 8
%ae41215 = call %struct.ScmObj* @const_init_int(i64 0)
%args47714$k40558$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49601 = alloca %struct.ScmObj*, align 8
%args47714$k40558$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40149, %struct.ScmObj* %args47714$k40558$0)
store volatile %struct.ScmObj* %args47714$k40558$1, %struct.ScmObj** %stackaddr$prim49601, align 8
%stackaddr$prim49602 = alloca %struct.ScmObj*, align 8
%args47714$k40558$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41215, %struct.ScmObj* %args47714$k40558$1)
store volatile %struct.ScmObj* %args47714$k40558$2, %struct.ScmObj** %stackaddr$prim49602, align 8
%clofunc49603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40558)
musttail call tailcc void %clofunc49603(%struct.ScmObj* %k40558, %struct.ScmObj* %args47714$k40558$2)
ret void
}

define tailcc void @proc_clo$ae41131(%struct.ScmObj* %env$ae41131,%struct.ScmObj* %current_45args47718) {
%stackaddr$prim49604 = alloca %struct.ScmObj*, align 8
%k40559 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47718)
store volatile %struct.ScmObj* %k40559, %struct.ScmObj** %stackaddr$prim49604, align 8
%stackaddr$prim49605 = alloca %struct.ScmObj*, align 8
%current_45args47719 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47718)
store volatile %struct.ScmObj* %current_45args47719, %struct.ScmObj** %stackaddr$prim49605, align 8
%stackaddr$prim49606 = alloca %struct.ScmObj*, align 8
%_37foldl140109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47719)
store volatile %struct.ScmObj* %_37foldl140109, %struct.ScmObj** %stackaddr$prim49606, align 8
%ae41133 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49607 = alloca %struct.ScmObj*, align 8
%fptrToInt49608 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41134 to i64
%ae41134 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49608)
store volatile %struct.ScmObj* %ae41134, %struct.ScmObj** %stackaddr$makeclosure49607, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41134, %struct.ScmObj* %_37foldl140109, i64 0)
%args47732$k40559$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49609 = alloca %struct.ScmObj*, align 8
%args47732$k40559$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41134, %struct.ScmObj* %args47732$k40559$0)
store volatile %struct.ScmObj* %args47732$k40559$1, %struct.ScmObj** %stackaddr$prim49609, align 8
%stackaddr$prim49610 = alloca %struct.ScmObj*, align 8
%args47732$k40559$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41133, %struct.ScmObj* %args47732$k40559$1)
store volatile %struct.ScmObj* %args47732$k40559$2, %struct.ScmObj** %stackaddr$prim49610, align 8
%clofunc49611 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40559)
musttail call tailcc void %clofunc49611(%struct.ScmObj* %k40559, %struct.ScmObj* %args47732$k40559$2)
ret void
}

define tailcc void @proc_clo$ae41134(%struct.ScmObj* %env$ae41134,%struct.ScmObj* %current_45args47721) {
%stackaddr$env-ref49612 = alloca %struct.ScmObj*, align 8
%_37foldl140109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41134, i64 0)
store %struct.ScmObj* %_37foldl140109, %struct.ScmObj** %stackaddr$env-ref49612
%stackaddr$prim49613 = alloca %struct.ScmObj*, align 8
%k40560 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47721)
store volatile %struct.ScmObj* %k40560, %struct.ScmObj** %stackaddr$prim49613, align 8
%stackaddr$prim49614 = alloca %struct.ScmObj*, align 8
%current_45args47722 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47721)
store volatile %struct.ScmObj* %current_45args47722, %struct.ScmObj** %stackaddr$prim49614, align 8
%stackaddr$prim49615 = alloca %struct.ScmObj*, align 8
%f40112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47722)
store volatile %struct.ScmObj* %f40112, %struct.ScmObj** %stackaddr$prim49615, align 8
%stackaddr$prim49616 = alloca %struct.ScmObj*, align 8
%current_45args47723 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47722)
store volatile %struct.ScmObj* %current_45args47723, %struct.ScmObj** %stackaddr$prim49616, align 8
%stackaddr$prim49617 = alloca %struct.ScmObj*, align 8
%acc40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47723)
store volatile %struct.ScmObj* %acc40111, %struct.ScmObj** %stackaddr$prim49617, align 8
%stackaddr$prim49618 = alloca %struct.ScmObj*, align 8
%current_45args47724 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47723)
store volatile %struct.ScmObj* %current_45args47724, %struct.ScmObj** %stackaddr$prim49618, align 8
%stackaddr$prim49619 = alloca %struct.ScmObj*, align 8
%lst40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47724)
store volatile %struct.ScmObj* %lst40110, %struct.ScmObj** %stackaddr$prim49619, align 8
%stackaddr$prim49620 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40110)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim49620, align 8
%truthy$cmp49621 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40270)
%cmp$cmp49621 = icmp eq i64 %truthy$cmp49621, 1
br i1 %cmp$cmp49621, label %truebranch$cmp49621, label %falsebranch$cmp49621
truebranch$cmp49621:
%ae41138 = call %struct.ScmObj* @const_init_int(i64 0)
%args47726$k40560$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49622 = alloca %struct.ScmObj*, align 8
%args47726$k40560$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40111, %struct.ScmObj* %args47726$k40560$0)
store volatile %struct.ScmObj* %args47726$k40560$1, %struct.ScmObj** %stackaddr$prim49622, align 8
%stackaddr$prim49623 = alloca %struct.ScmObj*, align 8
%args47726$k40560$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41138, %struct.ScmObj* %args47726$k40560$1)
store volatile %struct.ScmObj* %args47726$k40560$2, %struct.ScmObj** %stackaddr$prim49623, align 8
%clofunc49624 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40560)
musttail call tailcc void %clofunc49624(%struct.ScmObj* %k40560, %struct.ScmObj* %args47726$k40560$2)
ret void
falsebranch$cmp49621:
%stackaddr$prim49625 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40110)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim49625, align 8
%stackaddr$makeclosure49626 = alloca %struct.ScmObj*, align 8
%fptrToInt49627 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41145 to i64
%ae41145 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49627)
store volatile %struct.ScmObj* %ae41145, %struct.ScmObj** %stackaddr$makeclosure49626, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41145, %struct.ScmObj* %k40560, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41145, %struct.ScmObj* %f40112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41145, %struct.ScmObj* %lst40110, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41145, %struct.ScmObj* %_37foldl140109, i64 3)
%args47731$f40112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49628 = alloca %struct.ScmObj*, align 8
%args47731$f40112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40111, %struct.ScmObj* %args47731$f40112$0)
store volatile %struct.ScmObj* %args47731$f40112$1, %struct.ScmObj** %stackaddr$prim49628, align 8
%stackaddr$prim49629 = alloca %struct.ScmObj*, align 8
%args47731$f40112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40271, %struct.ScmObj* %args47731$f40112$1)
store volatile %struct.ScmObj* %args47731$f40112$2, %struct.ScmObj** %stackaddr$prim49629, align 8
%stackaddr$prim49630 = alloca %struct.ScmObj*, align 8
%args47731$f40112$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41145, %struct.ScmObj* %args47731$f40112$2)
store volatile %struct.ScmObj* %args47731$f40112$3, %struct.ScmObj** %stackaddr$prim49630, align 8
%clofunc49631 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40112)
musttail call tailcc void %clofunc49631(%struct.ScmObj* %f40112, %struct.ScmObj* %args47731$f40112$3)
ret void
}

define tailcc void @proc_clo$ae41145(%struct.ScmObj* %env$ae41145,%struct.ScmObj* %current_45args47727) {
%stackaddr$env-ref49632 = alloca %struct.ScmObj*, align 8
%k40560 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41145, i64 0)
store %struct.ScmObj* %k40560, %struct.ScmObj** %stackaddr$env-ref49632
%stackaddr$env-ref49633 = alloca %struct.ScmObj*, align 8
%f40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41145, i64 1)
store %struct.ScmObj* %f40112, %struct.ScmObj** %stackaddr$env-ref49633
%stackaddr$env-ref49634 = alloca %struct.ScmObj*, align 8
%lst40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41145, i64 2)
store %struct.ScmObj* %lst40110, %struct.ScmObj** %stackaddr$env-ref49634
%stackaddr$env-ref49635 = alloca %struct.ScmObj*, align 8
%_37foldl140109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41145, i64 3)
store %struct.ScmObj* %_37foldl140109, %struct.ScmObj** %stackaddr$env-ref49635
%stackaddr$prim49636 = alloca %struct.ScmObj*, align 8
%_95k40561 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47727)
store volatile %struct.ScmObj* %_95k40561, %struct.ScmObj** %stackaddr$prim49636, align 8
%stackaddr$prim49637 = alloca %struct.ScmObj*, align 8
%current_45args47728 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47727)
store volatile %struct.ScmObj* %current_45args47728, %struct.ScmObj** %stackaddr$prim49637, align 8
%stackaddr$prim49638 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47728)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim49638, align 8
%stackaddr$prim49639 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40110)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim49639, align 8
%args47730$_37foldl140109$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49640 = alloca %struct.ScmObj*, align 8
%args47730$_37foldl140109$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40273, %struct.ScmObj* %args47730$_37foldl140109$0)
store volatile %struct.ScmObj* %args47730$_37foldl140109$1, %struct.ScmObj** %stackaddr$prim49640, align 8
%stackaddr$prim49641 = alloca %struct.ScmObj*, align 8
%args47730$_37foldl140109$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40272, %struct.ScmObj* %args47730$_37foldl140109$1)
store volatile %struct.ScmObj* %args47730$_37foldl140109$2, %struct.ScmObj** %stackaddr$prim49641, align 8
%stackaddr$prim49642 = alloca %struct.ScmObj*, align 8
%args47730$_37foldl140109$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40112, %struct.ScmObj* %args47730$_37foldl140109$2)
store volatile %struct.ScmObj* %args47730$_37foldl140109$3, %struct.ScmObj** %stackaddr$prim49642, align 8
%stackaddr$prim49643 = alloca %struct.ScmObj*, align 8
%args47730$_37foldl140109$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40560, %struct.ScmObj* %args47730$_37foldl140109$3)
store volatile %struct.ScmObj* %args47730$_37foldl140109$4, %struct.ScmObj** %stackaddr$prim49643, align 8
%clofunc49644 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140109)
musttail call tailcc void %clofunc49644(%struct.ScmObj* %_37foldl140109, %struct.ScmObj* %args47730$_37foldl140109$4)
ret void
}

define tailcc void @proc_clo$ae41048(%struct.ScmObj* %env$ae41048,%struct.ScmObj* %current_45args47735) {
%stackaddr$prim49645 = alloca %struct.ScmObj*, align 8
%k40562 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47735)
store volatile %struct.ScmObj* %k40562, %struct.ScmObj** %stackaddr$prim49645, align 8
%stackaddr$prim49646 = alloca %struct.ScmObj*, align 8
%current_45args47736 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47735)
store volatile %struct.ScmObj* %current_45args47736, %struct.ScmObj** %stackaddr$prim49646, align 8
%stackaddr$prim49647 = alloca %struct.ScmObj*, align 8
%_37length40114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47736)
store volatile %struct.ScmObj* %_37length40114, %struct.ScmObj** %stackaddr$prim49647, align 8
%ae41050 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49648 = alloca %struct.ScmObj*, align 8
%fptrToInt49649 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41051 to i64
%ae41051 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49649)
store volatile %struct.ScmObj* %ae41051, %struct.ScmObj** %stackaddr$makeclosure49648, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41051, %struct.ScmObj* %_37length40114, i64 0)
%args47747$k40562$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49650 = alloca %struct.ScmObj*, align 8
%args47747$k40562$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41051, %struct.ScmObj* %args47747$k40562$0)
store volatile %struct.ScmObj* %args47747$k40562$1, %struct.ScmObj** %stackaddr$prim49650, align 8
%stackaddr$prim49651 = alloca %struct.ScmObj*, align 8
%args47747$k40562$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41050, %struct.ScmObj* %args47747$k40562$1)
store volatile %struct.ScmObj* %args47747$k40562$2, %struct.ScmObj** %stackaddr$prim49651, align 8
%clofunc49652 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40562)
musttail call tailcc void %clofunc49652(%struct.ScmObj* %k40562, %struct.ScmObj* %args47747$k40562$2)
ret void
}

define tailcc void @proc_clo$ae41051(%struct.ScmObj* %env$ae41051,%struct.ScmObj* %current_45args47738) {
%stackaddr$env-ref49653 = alloca %struct.ScmObj*, align 8
%_37length40114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41051, i64 0)
store %struct.ScmObj* %_37length40114, %struct.ScmObj** %stackaddr$env-ref49653
%stackaddr$prim49654 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47738)
store volatile %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$prim49654, align 8
%stackaddr$prim49655 = alloca %struct.ScmObj*, align 8
%current_45args47739 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47738)
store volatile %struct.ScmObj* %current_45args47739, %struct.ScmObj** %stackaddr$prim49655, align 8
%stackaddr$prim49656 = alloca %struct.ScmObj*, align 8
%lst40115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47739)
store volatile %struct.ScmObj* %lst40115, %struct.ScmObj** %stackaddr$prim49656, align 8
%stackaddr$prim49657 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40115)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim49657, align 8
%truthy$cmp49658 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40266)
%cmp$cmp49658 = icmp eq i64 %truthy$cmp49658, 1
br i1 %cmp$cmp49658, label %truebranch$cmp49658, label %falsebranch$cmp49658
truebranch$cmp49658:
%ae41055 = call %struct.ScmObj* @const_init_int(i64 0)
%ae41056 = call %struct.ScmObj* @const_init_int(i64 0)
%args47741$k40563$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49659 = alloca %struct.ScmObj*, align 8
%args47741$k40563$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41056, %struct.ScmObj* %args47741$k40563$0)
store volatile %struct.ScmObj* %args47741$k40563$1, %struct.ScmObj** %stackaddr$prim49659, align 8
%stackaddr$prim49660 = alloca %struct.ScmObj*, align 8
%args47741$k40563$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41055, %struct.ScmObj* %args47741$k40563$1)
store volatile %struct.ScmObj* %args47741$k40563$2, %struct.ScmObj** %stackaddr$prim49660, align 8
%clofunc49661 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40563)
musttail call tailcc void %clofunc49661(%struct.ScmObj* %k40563, %struct.ScmObj* %args47741$k40563$2)
ret void
falsebranch$cmp49658:
%stackaddr$prim49662 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40115)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim49662, align 8
%stackaddr$makeclosure49663 = alloca %struct.ScmObj*, align 8
%fptrToInt49664 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41065 to i64
%ae41065 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49664)
store volatile %struct.ScmObj* %ae41065, %struct.ScmObj** %stackaddr$makeclosure49663, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41065, %struct.ScmObj* %k40563, i64 0)
%args47746$_37length40114$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49665 = alloca %struct.ScmObj*, align 8
%args47746$_37length40114$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40267, %struct.ScmObj* %args47746$_37length40114$0)
store volatile %struct.ScmObj* %args47746$_37length40114$1, %struct.ScmObj** %stackaddr$prim49665, align 8
%stackaddr$prim49666 = alloca %struct.ScmObj*, align 8
%args47746$_37length40114$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41065, %struct.ScmObj* %args47746$_37length40114$1)
store volatile %struct.ScmObj* %args47746$_37length40114$2, %struct.ScmObj** %stackaddr$prim49666, align 8
%clofunc49667 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40114)
musttail call tailcc void %clofunc49667(%struct.ScmObj* %_37length40114, %struct.ScmObj* %args47746$_37length40114$2)
ret void
}

define tailcc void @proc_clo$ae41065(%struct.ScmObj* %env$ae41065,%struct.ScmObj* %current_45args47742) {
%stackaddr$env-ref49668 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41065, i64 0)
store %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$env-ref49668
%stackaddr$prim49669 = alloca %struct.ScmObj*, align 8
%_95k40564 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47742)
store volatile %struct.ScmObj* %_95k40564, %struct.ScmObj** %stackaddr$prim49669, align 8
%stackaddr$prim49670 = alloca %struct.ScmObj*, align 8
%current_45args47743 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47742)
store volatile %struct.ScmObj* %current_45args47743, %struct.ScmObj** %stackaddr$prim49670, align 8
%stackaddr$prim49671 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47743)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim49671, align 8
%ae41067 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49672 = alloca %struct.ScmObj*, align 8
%cpsprim40565 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae41067, %struct.ScmObj* %anf_45bind40268)
store volatile %struct.ScmObj* %cpsprim40565, %struct.ScmObj** %stackaddr$prim49672, align 8
%ae41070 = call %struct.ScmObj* @const_init_int(i64 0)
%args47745$k40563$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49673 = alloca %struct.ScmObj*, align 8
%args47745$k40563$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40565, %struct.ScmObj* %args47745$k40563$0)
store volatile %struct.ScmObj* %args47745$k40563$1, %struct.ScmObj** %stackaddr$prim49673, align 8
%stackaddr$prim49674 = alloca %struct.ScmObj*, align 8
%args47745$k40563$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41070, %struct.ScmObj* %args47745$k40563$1)
store volatile %struct.ScmObj* %args47745$k40563$2, %struct.ScmObj** %stackaddr$prim49674, align 8
%clofunc49675 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40563)
musttail call tailcc void %clofunc49675(%struct.ScmObj* %k40563, %struct.ScmObj* %args47745$k40563$2)
ret void
}

define tailcc void @proc_clo$ae40898(%struct.ScmObj* %env$ae40898,%struct.ScmObj* %current_45args47750) {
%stackaddr$prim49676 = alloca %struct.ScmObj*, align 8
%k40566 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47750)
store volatile %struct.ScmObj* %k40566, %struct.ScmObj** %stackaddr$prim49676, align 8
%stackaddr$prim49677 = alloca %struct.ScmObj*, align 8
%current_45args47751 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47750)
store volatile %struct.ScmObj* %current_45args47751, %struct.ScmObj** %stackaddr$prim49677, align 8
%stackaddr$prim49678 = alloca %struct.ScmObj*, align 8
%_37take40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47751)
store volatile %struct.ScmObj* %_37take40117, %struct.ScmObj** %stackaddr$prim49678, align 8
%ae40900 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49679 = alloca %struct.ScmObj*, align 8
%fptrToInt49680 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40901 to i64
%ae40901 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49680)
store volatile %struct.ScmObj* %ae40901, %struct.ScmObj** %stackaddr$makeclosure49679, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40901, %struct.ScmObj* %_37take40117, i64 0)
%args47764$k40566$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49681 = alloca %struct.ScmObj*, align 8
%args47764$k40566$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40901, %struct.ScmObj* %args47764$k40566$0)
store volatile %struct.ScmObj* %args47764$k40566$1, %struct.ScmObj** %stackaddr$prim49681, align 8
%stackaddr$prim49682 = alloca %struct.ScmObj*, align 8
%args47764$k40566$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40900, %struct.ScmObj* %args47764$k40566$1)
store volatile %struct.ScmObj* %args47764$k40566$2, %struct.ScmObj** %stackaddr$prim49682, align 8
%clofunc49683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40566)
musttail call tailcc void %clofunc49683(%struct.ScmObj* %k40566, %struct.ScmObj* %args47764$k40566$2)
ret void
}

define tailcc void @proc_clo$ae40901(%struct.ScmObj* %env$ae40901,%struct.ScmObj* %current_45args47753) {
%stackaddr$env-ref49684 = alloca %struct.ScmObj*, align 8
%_37take40117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40901, i64 0)
store %struct.ScmObj* %_37take40117, %struct.ScmObj** %stackaddr$env-ref49684
%stackaddr$prim49685 = alloca %struct.ScmObj*, align 8
%k40567 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47753)
store volatile %struct.ScmObj* %k40567, %struct.ScmObj** %stackaddr$prim49685, align 8
%stackaddr$prim49686 = alloca %struct.ScmObj*, align 8
%current_45args47754 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47753)
store volatile %struct.ScmObj* %current_45args47754, %struct.ScmObj** %stackaddr$prim49686, align 8
%stackaddr$prim49687 = alloca %struct.ScmObj*, align 8
%lst40119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47754)
store volatile %struct.ScmObj* %lst40119, %struct.ScmObj** %stackaddr$prim49687, align 8
%stackaddr$prim49688 = alloca %struct.ScmObj*, align 8
%current_45args47755 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47754)
store volatile %struct.ScmObj* %current_45args47755, %struct.ScmObj** %stackaddr$prim49688, align 8
%stackaddr$prim49689 = alloca %struct.ScmObj*, align 8
%n40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47755)
store volatile %struct.ScmObj* %n40118, %struct.ScmObj** %stackaddr$prim49689, align 8
%ae40903 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49690 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40118, %struct.ScmObj* %ae40903)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim49690, align 8
%truthy$cmp49691 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40259)
%cmp$cmp49691 = icmp eq i64 %truthy$cmp49691, 1
br i1 %cmp$cmp49691, label %truebranch$cmp49691, label %falsebranch$cmp49691
truebranch$cmp49691:
%ae40906 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40907 = call %struct.ScmObj* @const_init_null()
%args47757$k40567$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49692 = alloca %struct.ScmObj*, align 8
%args47757$k40567$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40907, %struct.ScmObj* %args47757$k40567$0)
store volatile %struct.ScmObj* %args47757$k40567$1, %struct.ScmObj** %stackaddr$prim49692, align 8
%stackaddr$prim49693 = alloca %struct.ScmObj*, align 8
%args47757$k40567$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40906, %struct.ScmObj* %args47757$k40567$1)
store volatile %struct.ScmObj* %args47757$k40567$2, %struct.ScmObj** %stackaddr$prim49693, align 8
%clofunc49694 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40567)
musttail call tailcc void %clofunc49694(%struct.ScmObj* %k40567, %struct.ScmObj* %args47757$k40567$2)
ret void
falsebranch$cmp49691:
%stackaddr$prim49695 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40119)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim49695, align 8
%truthy$cmp49696 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40260)
%cmp$cmp49696 = icmp eq i64 %truthy$cmp49696, 1
br i1 %cmp$cmp49696, label %truebranch$cmp49696, label %falsebranch$cmp49696
truebranch$cmp49696:
%ae40917 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40918 = call %struct.ScmObj* @const_init_null()
%args47758$k40567$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49697 = alloca %struct.ScmObj*, align 8
%args47758$k40567$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40918, %struct.ScmObj* %args47758$k40567$0)
store volatile %struct.ScmObj* %args47758$k40567$1, %struct.ScmObj** %stackaddr$prim49697, align 8
%stackaddr$prim49698 = alloca %struct.ScmObj*, align 8
%args47758$k40567$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40917, %struct.ScmObj* %args47758$k40567$1)
store volatile %struct.ScmObj* %args47758$k40567$2, %struct.ScmObj** %stackaddr$prim49698, align 8
%clofunc49699 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40567)
musttail call tailcc void %clofunc49699(%struct.ScmObj* %k40567, %struct.ScmObj* %args47758$k40567$2)
ret void
falsebranch$cmp49696:
%stackaddr$prim49700 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40119)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim49700, align 8
%stackaddr$prim49701 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40119)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim49701, align 8
%ae40928 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49702 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40118, %struct.ScmObj* %ae40928)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim49702, align 8
%stackaddr$makeclosure49703 = alloca %struct.ScmObj*, align 8
%fptrToInt49704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40930 to i64
%ae40930 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49704)
store volatile %struct.ScmObj* %ae40930, %struct.ScmObj** %stackaddr$makeclosure49703, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40930, %struct.ScmObj* %k40567, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40930, %struct.ScmObj* %anf_45bind40261, i64 1)
%args47763$_37take40117$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49705 = alloca %struct.ScmObj*, align 8
%args47763$_37take40117$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40263, %struct.ScmObj* %args47763$_37take40117$0)
store volatile %struct.ScmObj* %args47763$_37take40117$1, %struct.ScmObj** %stackaddr$prim49705, align 8
%stackaddr$prim49706 = alloca %struct.ScmObj*, align 8
%args47763$_37take40117$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40262, %struct.ScmObj* %args47763$_37take40117$1)
store volatile %struct.ScmObj* %args47763$_37take40117$2, %struct.ScmObj** %stackaddr$prim49706, align 8
%stackaddr$prim49707 = alloca %struct.ScmObj*, align 8
%args47763$_37take40117$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40930, %struct.ScmObj* %args47763$_37take40117$2)
store volatile %struct.ScmObj* %args47763$_37take40117$3, %struct.ScmObj** %stackaddr$prim49707, align 8
%clofunc49708 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40117)
musttail call tailcc void %clofunc49708(%struct.ScmObj* %_37take40117, %struct.ScmObj* %args47763$_37take40117$3)
ret void
}

define tailcc void @proc_clo$ae40930(%struct.ScmObj* %env$ae40930,%struct.ScmObj* %current_45args47759) {
%stackaddr$env-ref49709 = alloca %struct.ScmObj*, align 8
%k40567 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40930, i64 0)
store %struct.ScmObj* %k40567, %struct.ScmObj** %stackaddr$env-ref49709
%stackaddr$env-ref49710 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40930, i64 1)
store %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$env-ref49710
%stackaddr$prim49711 = alloca %struct.ScmObj*, align 8
%_95k40568 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47759)
store volatile %struct.ScmObj* %_95k40568, %struct.ScmObj** %stackaddr$prim49711, align 8
%stackaddr$prim49712 = alloca %struct.ScmObj*, align 8
%current_45args47760 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47759)
store volatile %struct.ScmObj* %current_45args47760, %struct.ScmObj** %stackaddr$prim49712, align 8
%stackaddr$prim49713 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47760)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim49713, align 8
%stackaddr$prim49714 = alloca %struct.ScmObj*, align 8
%cpsprim40569 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40261, %struct.ScmObj* %anf_45bind40264)
store volatile %struct.ScmObj* %cpsprim40569, %struct.ScmObj** %stackaddr$prim49714, align 8
%ae40936 = call %struct.ScmObj* @const_init_int(i64 0)
%args47762$k40567$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49715 = alloca %struct.ScmObj*, align 8
%args47762$k40567$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40569, %struct.ScmObj* %args47762$k40567$0)
store volatile %struct.ScmObj* %args47762$k40567$1, %struct.ScmObj** %stackaddr$prim49715, align 8
%stackaddr$prim49716 = alloca %struct.ScmObj*, align 8
%args47762$k40567$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40936, %struct.ScmObj* %args47762$k40567$1)
store volatile %struct.ScmObj* %args47762$k40567$2, %struct.ScmObj** %stackaddr$prim49716, align 8
%clofunc49717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40567)
musttail call tailcc void %clofunc49717(%struct.ScmObj* %k40567, %struct.ScmObj* %args47762$k40567$2)
ret void
}

define tailcc void @proc_clo$ae40801(%struct.ScmObj* %env$ae40801,%struct.ScmObj* %current_45args47767) {
%stackaddr$prim49718 = alloca %struct.ScmObj*, align 8
%k40570 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47767)
store volatile %struct.ScmObj* %k40570, %struct.ScmObj** %stackaddr$prim49718, align 8
%stackaddr$prim49719 = alloca %struct.ScmObj*, align 8
%current_45args47768 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47767)
store volatile %struct.ScmObj* %current_45args47768, %struct.ScmObj** %stackaddr$prim49719, align 8
%stackaddr$prim49720 = alloca %struct.ScmObj*, align 8
%_37map40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47768)
store volatile %struct.ScmObj* %_37map40121, %struct.ScmObj** %stackaddr$prim49720, align 8
%ae40803 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49721 = alloca %struct.ScmObj*, align 8
%fptrToInt49722 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40804 to i64
%ae40804 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49722)
store volatile %struct.ScmObj* %ae40804, %struct.ScmObj** %stackaddr$makeclosure49721, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40804, %struct.ScmObj* %_37map40121, i64 0)
%args47784$k40570$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49723 = alloca %struct.ScmObj*, align 8
%args47784$k40570$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40804, %struct.ScmObj* %args47784$k40570$0)
store volatile %struct.ScmObj* %args47784$k40570$1, %struct.ScmObj** %stackaddr$prim49723, align 8
%stackaddr$prim49724 = alloca %struct.ScmObj*, align 8
%args47784$k40570$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40803, %struct.ScmObj* %args47784$k40570$1)
store volatile %struct.ScmObj* %args47784$k40570$2, %struct.ScmObj** %stackaddr$prim49724, align 8
%clofunc49725 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40570)
musttail call tailcc void %clofunc49725(%struct.ScmObj* %k40570, %struct.ScmObj* %args47784$k40570$2)
ret void
}

define tailcc void @proc_clo$ae40804(%struct.ScmObj* %env$ae40804,%struct.ScmObj* %current_45args47770) {
%stackaddr$env-ref49726 = alloca %struct.ScmObj*, align 8
%_37map40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40804, i64 0)
store %struct.ScmObj* %_37map40121, %struct.ScmObj** %stackaddr$env-ref49726
%stackaddr$prim49727 = alloca %struct.ScmObj*, align 8
%k40571 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47770)
store volatile %struct.ScmObj* %k40571, %struct.ScmObj** %stackaddr$prim49727, align 8
%stackaddr$prim49728 = alloca %struct.ScmObj*, align 8
%current_45args47771 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47770)
store volatile %struct.ScmObj* %current_45args47771, %struct.ScmObj** %stackaddr$prim49728, align 8
%stackaddr$prim49729 = alloca %struct.ScmObj*, align 8
%f40123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47771)
store volatile %struct.ScmObj* %f40123, %struct.ScmObj** %stackaddr$prim49729, align 8
%stackaddr$prim49730 = alloca %struct.ScmObj*, align 8
%current_45args47772 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47771)
store volatile %struct.ScmObj* %current_45args47772, %struct.ScmObj** %stackaddr$prim49730, align 8
%stackaddr$prim49731 = alloca %struct.ScmObj*, align 8
%lst40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47772)
store volatile %struct.ScmObj* %lst40122, %struct.ScmObj** %stackaddr$prim49731, align 8
%stackaddr$prim49732 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim49732, align 8
%truthy$cmp49733 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40253)
%cmp$cmp49733 = icmp eq i64 %truthy$cmp49733, 1
br i1 %cmp$cmp49733, label %truebranch$cmp49733, label %falsebranch$cmp49733
truebranch$cmp49733:
%ae40808 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40809 = call %struct.ScmObj* @const_init_null()
%args47774$k40571$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49734 = alloca %struct.ScmObj*, align 8
%args47774$k40571$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40809, %struct.ScmObj* %args47774$k40571$0)
store volatile %struct.ScmObj* %args47774$k40571$1, %struct.ScmObj** %stackaddr$prim49734, align 8
%stackaddr$prim49735 = alloca %struct.ScmObj*, align 8
%args47774$k40571$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40808, %struct.ScmObj* %args47774$k40571$1)
store volatile %struct.ScmObj* %args47774$k40571$2, %struct.ScmObj** %stackaddr$prim49735, align 8
%clofunc49736 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40571)
musttail call tailcc void %clofunc49736(%struct.ScmObj* %k40571, %struct.ScmObj* %args47774$k40571$2)
ret void
falsebranch$cmp49733:
%stackaddr$prim49737 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim49737, align 8
%stackaddr$makeclosure49738 = alloca %struct.ScmObj*, align 8
%fptrToInt49739 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40818 to i64
%ae40818 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49739)
store volatile %struct.ScmObj* %ae40818, %struct.ScmObj** %stackaddr$makeclosure49738, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40818, %struct.ScmObj* %f40123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40818, %struct.ScmObj* %k40571, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40818, %struct.ScmObj* %lst40122, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40818, %struct.ScmObj* %_37map40121, i64 3)
%args47783$f40123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49740 = alloca %struct.ScmObj*, align 8
%args47783$f40123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40254, %struct.ScmObj* %args47783$f40123$0)
store volatile %struct.ScmObj* %args47783$f40123$1, %struct.ScmObj** %stackaddr$prim49740, align 8
%stackaddr$prim49741 = alloca %struct.ScmObj*, align 8
%args47783$f40123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40818, %struct.ScmObj* %args47783$f40123$1)
store volatile %struct.ScmObj* %args47783$f40123$2, %struct.ScmObj** %stackaddr$prim49741, align 8
%clofunc49742 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40123)
musttail call tailcc void %clofunc49742(%struct.ScmObj* %f40123, %struct.ScmObj* %args47783$f40123$2)
ret void
}

define tailcc void @proc_clo$ae40818(%struct.ScmObj* %env$ae40818,%struct.ScmObj* %current_45args47775) {
%stackaddr$env-ref49743 = alloca %struct.ScmObj*, align 8
%f40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40818, i64 0)
store %struct.ScmObj* %f40123, %struct.ScmObj** %stackaddr$env-ref49743
%stackaddr$env-ref49744 = alloca %struct.ScmObj*, align 8
%k40571 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40818, i64 1)
store %struct.ScmObj* %k40571, %struct.ScmObj** %stackaddr$env-ref49744
%stackaddr$env-ref49745 = alloca %struct.ScmObj*, align 8
%lst40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40818, i64 2)
store %struct.ScmObj* %lst40122, %struct.ScmObj** %stackaddr$env-ref49745
%stackaddr$env-ref49746 = alloca %struct.ScmObj*, align 8
%_37map40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40818, i64 3)
store %struct.ScmObj* %_37map40121, %struct.ScmObj** %stackaddr$env-ref49746
%stackaddr$prim49747 = alloca %struct.ScmObj*, align 8
%_95k40572 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47775)
store volatile %struct.ScmObj* %_95k40572, %struct.ScmObj** %stackaddr$prim49747, align 8
%stackaddr$prim49748 = alloca %struct.ScmObj*, align 8
%current_45args47776 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47775)
store volatile %struct.ScmObj* %current_45args47776, %struct.ScmObj** %stackaddr$prim49748, align 8
%stackaddr$prim49749 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47776)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim49749, align 8
%stackaddr$prim49750 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim49750, align 8
%stackaddr$makeclosure49751 = alloca %struct.ScmObj*, align 8
%fptrToInt49752 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40822 to i64
%ae40822 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49752)
store volatile %struct.ScmObj* %ae40822, %struct.ScmObj** %stackaddr$makeclosure49751, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40822, %struct.ScmObj* %anf_45bind40255, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40822, %struct.ScmObj* %k40571, i64 1)
%args47782$_37map40121$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49753 = alloca %struct.ScmObj*, align 8
%args47782$_37map40121$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40256, %struct.ScmObj* %args47782$_37map40121$0)
store volatile %struct.ScmObj* %args47782$_37map40121$1, %struct.ScmObj** %stackaddr$prim49753, align 8
%stackaddr$prim49754 = alloca %struct.ScmObj*, align 8
%args47782$_37map40121$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40123, %struct.ScmObj* %args47782$_37map40121$1)
store volatile %struct.ScmObj* %args47782$_37map40121$2, %struct.ScmObj** %stackaddr$prim49754, align 8
%stackaddr$prim49755 = alloca %struct.ScmObj*, align 8
%args47782$_37map40121$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40822, %struct.ScmObj* %args47782$_37map40121$2)
store volatile %struct.ScmObj* %args47782$_37map40121$3, %struct.ScmObj** %stackaddr$prim49755, align 8
%clofunc49756 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40121)
musttail call tailcc void %clofunc49756(%struct.ScmObj* %_37map40121, %struct.ScmObj* %args47782$_37map40121$3)
ret void
}

define tailcc void @proc_clo$ae40822(%struct.ScmObj* %env$ae40822,%struct.ScmObj* %current_45args47778) {
%stackaddr$env-ref49757 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40822, i64 0)
store %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$env-ref49757
%stackaddr$env-ref49758 = alloca %struct.ScmObj*, align 8
%k40571 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40822, i64 1)
store %struct.ScmObj* %k40571, %struct.ScmObj** %stackaddr$env-ref49758
%stackaddr$prim49759 = alloca %struct.ScmObj*, align 8
%_95k40573 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47778)
store volatile %struct.ScmObj* %_95k40573, %struct.ScmObj** %stackaddr$prim49759, align 8
%stackaddr$prim49760 = alloca %struct.ScmObj*, align 8
%current_45args47779 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47778)
store volatile %struct.ScmObj* %current_45args47779, %struct.ScmObj** %stackaddr$prim49760, align 8
%stackaddr$prim49761 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47779)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim49761, align 8
%stackaddr$prim49762 = alloca %struct.ScmObj*, align 8
%cpsprim40574 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40255, %struct.ScmObj* %anf_45bind40257)
store volatile %struct.ScmObj* %cpsprim40574, %struct.ScmObj** %stackaddr$prim49762, align 8
%ae40828 = call %struct.ScmObj* @const_init_int(i64 0)
%args47781$k40571$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49763 = alloca %struct.ScmObj*, align 8
%args47781$k40571$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40574, %struct.ScmObj* %args47781$k40571$0)
store volatile %struct.ScmObj* %args47781$k40571$1, %struct.ScmObj** %stackaddr$prim49763, align 8
%stackaddr$prim49764 = alloca %struct.ScmObj*, align 8
%args47781$k40571$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40828, %struct.ScmObj* %args47781$k40571$1)
store volatile %struct.ScmObj* %args47781$k40571$2, %struct.ScmObj** %stackaddr$prim49764, align 8
%clofunc49765 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40571)
musttail call tailcc void %clofunc49765(%struct.ScmObj* %k40571, %struct.ScmObj* %args47781$k40571$2)
ret void
}

define tailcc void @proc_clo$ae40721(%struct.ScmObj* %env$ae40721,%struct.ScmObj* %current_45args47787) {
%stackaddr$prim49766 = alloca %struct.ScmObj*, align 8
%k40575 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47787)
store volatile %struct.ScmObj* %k40575, %struct.ScmObj** %stackaddr$prim49766, align 8
%stackaddr$prim49767 = alloca %struct.ScmObj*, align 8
%current_45args47788 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47787)
store volatile %struct.ScmObj* %current_45args47788, %struct.ScmObj** %stackaddr$prim49767, align 8
%stackaddr$prim49768 = alloca %struct.ScmObj*, align 8
%_37foldr140125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47788)
store volatile %struct.ScmObj* %_37foldr140125, %struct.ScmObj** %stackaddr$prim49768, align 8
%ae40723 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49769 = alloca %struct.ScmObj*, align 8
%fptrToInt49770 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40724 to i64
%ae40724 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49770)
store volatile %struct.ScmObj* %ae40724, %struct.ScmObj** %stackaddr$makeclosure49769, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40724, %struct.ScmObj* %_37foldr140125, i64 0)
%args47801$k40575$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49771 = alloca %struct.ScmObj*, align 8
%args47801$k40575$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40724, %struct.ScmObj* %args47801$k40575$0)
store volatile %struct.ScmObj* %args47801$k40575$1, %struct.ScmObj** %stackaddr$prim49771, align 8
%stackaddr$prim49772 = alloca %struct.ScmObj*, align 8
%args47801$k40575$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40723, %struct.ScmObj* %args47801$k40575$1)
store volatile %struct.ScmObj* %args47801$k40575$2, %struct.ScmObj** %stackaddr$prim49772, align 8
%clofunc49773 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40575)
musttail call tailcc void %clofunc49773(%struct.ScmObj* %k40575, %struct.ScmObj* %args47801$k40575$2)
ret void
}

define tailcc void @proc_clo$ae40724(%struct.ScmObj* %env$ae40724,%struct.ScmObj* %current_45args47790) {
%stackaddr$env-ref49774 = alloca %struct.ScmObj*, align 8
%_37foldr140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40724, i64 0)
store %struct.ScmObj* %_37foldr140125, %struct.ScmObj** %stackaddr$env-ref49774
%stackaddr$prim49775 = alloca %struct.ScmObj*, align 8
%k40576 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47790)
store volatile %struct.ScmObj* %k40576, %struct.ScmObj** %stackaddr$prim49775, align 8
%stackaddr$prim49776 = alloca %struct.ScmObj*, align 8
%current_45args47791 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47790)
store volatile %struct.ScmObj* %current_45args47791, %struct.ScmObj** %stackaddr$prim49776, align 8
%stackaddr$prim49777 = alloca %struct.ScmObj*, align 8
%f40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47791)
store volatile %struct.ScmObj* %f40128, %struct.ScmObj** %stackaddr$prim49777, align 8
%stackaddr$prim49778 = alloca %struct.ScmObj*, align 8
%current_45args47792 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47791)
store volatile %struct.ScmObj* %current_45args47792, %struct.ScmObj** %stackaddr$prim49778, align 8
%stackaddr$prim49779 = alloca %struct.ScmObj*, align 8
%acc40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47792)
store volatile %struct.ScmObj* %acc40127, %struct.ScmObj** %stackaddr$prim49779, align 8
%stackaddr$prim49780 = alloca %struct.ScmObj*, align 8
%current_45args47793 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47792)
store volatile %struct.ScmObj* %current_45args47793, %struct.ScmObj** %stackaddr$prim49780, align 8
%stackaddr$prim49781 = alloca %struct.ScmObj*, align 8
%lst40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47793)
store volatile %struct.ScmObj* %lst40126, %struct.ScmObj** %stackaddr$prim49781, align 8
%stackaddr$prim49782 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$prim49782, align 8
%truthy$cmp49783 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40248)
%cmp$cmp49783 = icmp eq i64 %truthy$cmp49783, 1
br i1 %cmp$cmp49783, label %truebranch$cmp49783, label %falsebranch$cmp49783
truebranch$cmp49783:
%ae40728 = call %struct.ScmObj* @const_init_int(i64 0)
%args47795$k40576$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49784 = alloca %struct.ScmObj*, align 8
%args47795$k40576$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40127, %struct.ScmObj* %args47795$k40576$0)
store volatile %struct.ScmObj* %args47795$k40576$1, %struct.ScmObj** %stackaddr$prim49784, align 8
%stackaddr$prim49785 = alloca %struct.ScmObj*, align 8
%args47795$k40576$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40728, %struct.ScmObj* %args47795$k40576$1)
store volatile %struct.ScmObj* %args47795$k40576$2, %struct.ScmObj** %stackaddr$prim49785, align 8
%clofunc49786 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40576)
musttail call tailcc void %clofunc49786(%struct.ScmObj* %k40576, %struct.ScmObj* %args47795$k40576$2)
ret void
falsebranch$cmp49783:
%stackaddr$prim49787 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$prim49787, align 8
%stackaddr$prim49788 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$prim49788, align 8
%stackaddr$makeclosure49789 = alloca %struct.ScmObj*, align 8
%fptrToInt49790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40736 to i64
%ae40736 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49790)
store volatile %struct.ScmObj* %ae40736, %struct.ScmObj** %stackaddr$makeclosure49789, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40736, %struct.ScmObj* %f40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40736, %struct.ScmObj* %k40576, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40736, %struct.ScmObj* %anf_45bind40249, i64 2)
%args47800$_37foldr140125$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49791 = alloca %struct.ScmObj*, align 8
%args47800$_37foldr140125$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40250, %struct.ScmObj* %args47800$_37foldr140125$0)
store volatile %struct.ScmObj* %args47800$_37foldr140125$1, %struct.ScmObj** %stackaddr$prim49791, align 8
%stackaddr$prim49792 = alloca %struct.ScmObj*, align 8
%args47800$_37foldr140125$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40127, %struct.ScmObj* %args47800$_37foldr140125$1)
store volatile %struct.ScmObj* %args47800$_37foldr140125$2, %struct.ScmObj** %stackaddr$prim49792, align 8
%stackaddr$prim49793 = alloca %struct.ScmObj*, align 8
%args47800$_37foldr140125$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40128, %struct.ScmObj* %args47800$_37foldr140125$2)
store volatile %struct.ScmObj* %args47800$_37foldr140125$3, %struct.ScmObj** %stackaddr$prim49793, align 8
%stackaddr$prim49794 = alloca %struct.ScmObj*, align 8
%args47800$_37foldr140125$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40736, %struct.ScmObj* %args47800$_37foldr140125$3)
store volatile %struct.ScmObj* %args47800$_37foldr140125$4, %struct.ScmObj** %stackaddr$prim49794, align 8
%clofunc49795 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140125)
musttail call tailcc void %clofunc49795(%struct.ScmObj* %_37foldr140125, %struct.ScmObj* %args47800$_37foldr140125$4)
ret void
}

define tailcc void @proc_clo$ae40736(%struct.ScmObj* %env$ae40736,%struct.ScmObj* %current_45args47796) {
%stackaddr$env-ref49796 = alloca %struct.ScmObj*, align 8
%f40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40736, i64 0)
store %struct.ScmObj* %f40128, %struct.ScmObj** %stackaddr$env-ref49796
%stackaddr$env-ref49797 = alloca %struct.ScmObj*, align 8
%k40576 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40736, i64 1)
store %struct.ScmObj* %k40576, %struct.ScmObj** %stackaddr$env-ref49797
%stackaddr$env-ref49798 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40736, i64 2)
store %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$env-ref49798
%stackaddr$prim49799 = alloca %struct.ScmObj*, align 8
%_95k40577 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47796)
store volatile %struct.ScmObj* %_95k40577, %struct.ScmObj** %stackaddr$prim49799, align 8
%stackaddr$prim49800 = alloca %struct.ScmObj*, align 8
%current_45args47797 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47796)
store volatile %struct.ScmObj* %current_45args47797, %struct.ScmObj** %stackaddr$prim49800, align 8
%stackaddr$prim49801 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47797)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim49801, align 8
%args47799$f40128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49802 = alloca %struct.ScmObj*, align 8
%args47799$f40128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40251, %struct.ScmObj* %args47799$f40128$0)
store volatile %struct.ScmObj* %args47799$f40128$1, %struct.ScmObj** %stackaddr$prim49802, align 8
%stackaddr$prim49803 = alloca %struct.ScmObj*, align 8
%args47799$f40128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40249, %struct.ScmObj* %args47799$f40128$1)
store volatile %struct.ScmObj* %args47799$f40128$2, %struct.ScmObj** %stackaddr$prim49803, align 8
%stackaddr$prim49804 = alloca %struct.ScmObj*, align 8
%args47799$f40128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40576, %struct.ScmObj* %args47799$f40128$2)
store volatile %struct.ScmObj* %args47799$f40128$3, %struct.ScmObj** %stackaddr$prim49804, align 8
%clofunc49805 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40128)
musttail call tailcc void %clofunc49805(%struct.ScmObj* %f40128, %struct.ScmObj* %args47799$f40128$3)
ret void
}

define tailcc void @proc_clo$ae40604(%struct.ScmObj* %env$ae40604,%struct.ScmObj* %current_45args47804) {
%stackaddr$prim49806 = alloca %struct.ScmObj*, align 8
%k40578 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47804)
store volatile %struct.ScmObj* %k40578, %struct.ScmObj** %stackaddr$prim49806, align 8
%stackaddr$prim49807 = alloca %struct.ScmObj*, align 8
%current_45args47805 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47804)
store volatile %struct.ScmObj* %current_45args47805, %struct.ScmObj** %stackaddr$prim49807, align 8
%stackaddr$prim49808 = alloca %struct.ScmObj*, align 8
%y40105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47805)
store volatile %struct.ScmObj* %y40105, %struct.ScmObj** %stackaddr$prim49808, align 8
%ae40606 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49809 = alloca %struct.ScmObj*, align 8
%fptrToInt49810 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40607 to i64
%ae40607 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49810)
store volatile %struct.ScmObj* %ae40607, %struct.ScmObj** %stackaddr$makeclosure49809, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40607, %struct.ScmObj* %y40105, i64 0)
%args47823$k40578$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49811 = alloca %struct.ScmObj*, align 8
%args47823$k40578$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40607, %struct.ScmObj* %args47823$k40578$0)
store volatile %struct.ScmObj* %args47823$k40578$1, %struct.ScmObj** %stackaddr$prim49811, align 8
%stackaddr$prim49812 = alloca %struct.ScmObj*, align 8
%args47823$k40578$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40606, %struct.ScmObj* %args47823$k40578$1)
store volatile %struct.ScmObj* %args47823$k40578$2, %struct.ScmObj** %stackaddr$prim49812, align 8
%clofunc49813 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40578)
musttail call tailcc void %clofunc49813(%struct.ScmObj* %k40578, %struct.ScmObj* %args47823$k40578$2)
ret void
}

define tailcc void @proc_clo$ae40607(%struct.ScmObj* %env$ae40607,%struct.ScmObj* %current_45args47807) {
%stackaddr$env-ref49814 = alloca %struct.ScmObj*, align 8
%y40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40607, i64 0)
store %struct.ScmObj* %y40105, %struct.ScmObj** %stackaddr$env-ref49814
%stackaddr$prim49815 = alloca %struct.ScmObj*, align 8
%k40579 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47807)
store volatile %struct.ScmObj* %k40579, %struct.ScmObj** %stackaddr$prim49815, align 8
%stackaddr$prim49816 = alloca %struct.ScmObj*, align 8
%current_45args47808 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47807)
store volatile %struct.ScmObj* %current_45args47808, %struct.ScmObj** %stackaddr$prim49816, align 8
%stackaddr$prim49817 = alloca %struct.ScmObj*, align 8
%f40106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47808)
store volatile %struct.ScmObj* %f40106, %struct.ScmObj** %stackaddr$prim49817, align 8
%stackaddr$makeclosure49818 = alloca %struct.ScmObj*, align 8
%fptrToInt49819 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40608 to i64
%ae40608 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49819)
store volatile %struct.ScmObj* %ae40608, %struct.ScmObj** %stackaddr$makeclosure49818, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40608, %struct.ScmObj* %k40579, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40608, %struct.ScmObj* %f40106, i64 1)
%ae40609 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49820 = alloca %struct.ScmObj*, align 8
%fptrToInt49821 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40610 to i64
%ae40610 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49821)
store volatile %struct.ScmObj* %ae40610, %struct.ScmObj** %stackaddr$makeclosure49820, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40610, %struct.ScmObj* %f40106, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40610, %struct.ScmObj* %y40105, i64 1)
%args47822$ae40608$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49822 = alloca %struct.ScmObj*, align 8
%args47822$ae40608$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40610, %struct.ScmObj* %args47822$ae40608$0)
store volatile %struct.ScmObj* %args47822$ae40608$1, %struct.ScmObj** %stackaddr$prim49822, align 8
%stackaddr$prim49823 = alloca %struct.ScmObj*, align 8
%args47822$ae40608$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40609, %struct.ScmObj* %args47822$ae40608$1)
store volatile %struct.ScmObj* %args47822$ae40608$2, %struct.ScmObj** %stackaddr$prim49823, align 8
%clofunc49824 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40608)
musttail call tailcc void %clofunc49824(%struct.ScmObj* %ae40608, %struct.ScmObj* %args47822$ae40608$2)
ret void
}

define tailcc void @proc_clo$ae40608(%struct.ScmObj* %env$ae40608,%struct.ScmObj* %current_45args47810) {
%stackaddr$env-ref49825 = alloca %struct.ScmObj*, align 8
%k40579 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40608, i64 0)
store %struct.ScmObj* %k40579, %struct.ScmObj** %stackaddr$env-ref49825
%stackaddr$env-ref49826 = alloca %struct.ScmObj*, align 8
%f40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40608, i64 1)
store %struct.ScmObj* %f40106, %struct.ScmObj** %stackaddr$env-ref49826
%stackaddr$prim49827 = alloca %struct.ScmObj*, align 8
%_95k40580 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47810)
store volatile %struct.ScmObj* %_95k40580, %struct.ScmObj** %stackaddr$prim49827, align 8
%stackaddr$prim49828 = alloca %struct.ScmObj*, align 8
%current_45args47811 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47810)
store volatile %struct.ScmObj* %current_45args47811, %struct.ScmObj** %stackaddr$prim49828, align 8
%stackaddr$prim49829 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47811)
store volatile %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$prim49829, align 8
%args47813$f40106$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49830 = alloca %struct.ScmObj*, align 8
%args47813$f40106$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40246, %struct.ScmObj* %args47813$f40106$0)
store volatile %struct.ScmObj* %args47813$f40106$1, %struct.ScmObj** %stackaddr$prim49830, align 8
%stackaddr$prim49831 = alloca %struct.ScmObj*, align 8
%args47813$f40106$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40579, %struct.ScmObj* %args47813$f40106$1)
store volatile %struct.ScmObj* %args47813$f40106$2, %struct.ScmObj** %stackaddr$prim49831, align 8
%clofunc49832 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40106)
musttail call tailcc void %clofunc49832(%struct.ScmObj* %f40106, %struct.ScmObj* %args47813$f40106$2)
ret void
}

define tailcc void @proc_clo$ae40610(%struct.ScmObj* %env$ae40610,%struct.ScmObj* %args4010740581) {
%stackaddr$env-ref49833 = alloca %struct.ScmObj*, align 8
%f40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40610, i64 0)
store %struct.ScmObj* %f40106, %struct.ScmObj** %stackaddr$env-ref49833
%stackaddr$env-ref49834 = alloca %struct.ScmObj*, align 8
%y40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40610, i64 1)
store %struct.ScmObj* %y40105, %struct.ScmObj** %stackaddr$env-ref49834
%stackaddr$prim49835 = alloca %struct.ScmObj*, align 8
%k40582 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4010740581)
store volatile %struct.ScmObj* %k40582, %struct.ScmObj** %stackaddr$prim49835, align 8
%stackaddr$prim49836 = alloca %struct.ScmObj*, align 8
%args40107 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4010740581)
store volatile %struct.ScmObj* %args40107, %struct.ScmObj** %stackaddr$prim49836, align 8
%stackaddr$makeclosure49837 = alloca %struct.ScmObj*, align 8
%fptrToInt49838 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40614 to i64
%ae40614 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49838)
store volatile %struct.ScmObj* %ae40614, %struct.ScmObj** %stackaddr$makeclosure49837, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40614, %struct.ScmObj* %k40582, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40614, %struct.ScmObj* %args40107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40614, %struct.ScmObj* %f40106, i64 2)
%args47821$y40105$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49839 = alloca %struct.ScmObj*, align 8
%args47821$y40105$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40105, %struct.ScmObj* %args47821$y40105$0)
store volatile %struct.ScmObj* %args47821$y40105$1, %struct.ScmObj** %stackaddr$prim49839, align 8
%stackaddr$prim49840 = alloca %struct.ScmObj*, align 8
%args47821$y40105$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40614, %struct.ScmObj* %args47821$y40105$1)
store volatile %struct.ScmObj* %args47821$y40105$2, %struct.ScmObj** %stackaddr$prim49840, align 8
%clofunc49841 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40105)
musttail call tailcc void %clofunc49841(%struct.ScmObj* %y40105, %struct.ScmObj* %args47821$y40105$2)
ret void
}

define tailcc void @proc_clo$ae40614(%struct.ScmObj* %env$ae40614,%struct.ScmObj* %current_45args47814) {
%stackaddr$env-ref49842 = alloca %struct.ScmObj*, align 8
%k40582 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40614, i64 0)
store %struct.ScmObj* %k40582, %struct.ScmObj** %stackaddr$env-ref49842
%stackaddr$env-ref49843 = alloca %struct.ScmObj*, align 8
%args40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40614, i64 1)
store %struct.ScmObj* %args40107, %struct.ScmObj** %stackaddr$env-ref49843
%stackaddr$env-ref49844 = alloca %struct.ScmObj*, align 8
%f40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40614, i64 2)
store %struct.ScmObj* %f40106, %struct.ScmObj** %stackaddr$env-ref49844
%stackaddr$prim49845 = alloca %struct.ScmObj*, align 8
%_95k40583 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47814)
store volatile %struct.ScmObj* %_95k40583, %struct.ScmObj** %stackaddr$prim49845, align 8
%stackaddr$prim49846 = alloca %struct.ScmObj*, align 8
%current_45args47815 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47814)
store volatile %struct.ScmObj* %current_45args47815, %struct.ScmObj** %stackaddr$prim49846, align 8
%stackaddr$prim49847 = alloca %struct.ScmObj*, align 8
%anf_45bind40244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47815)
store volatile %struct.ScmObj* %anf_45bind40244, %struct.ScmObj** %stackaddr$prim49847, align 8
%stackaddr$makeclosure49848 = alloca %struct.ScmObj*, align 8
%fptrToInt49849 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40617 to i64
%ae40617 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49849)
store volatile %struct.ScmObj* %ae40617, %struct.ScmObj** %stackaddr$makeclosure49848, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40617, %struct.ScmObj* %k40582, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40617, %struct.ScmObj* %args40107, i64 1)
%args47820$anf_45bind40244$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49850 = alloca %struct.ScmObj*, align 8
%args47820$anf_45bind40244$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40106, %struct.ScmObj* %args47820$anf_45bind40244$0)
store volatile %struct.ScmObj* %args47820$anf_45bind40244$1, %struct.ScmObj** %stackaddr$prim49850, align 8
%stackaddr$prim49851 = alloca %struct.ScmObj*, align 8
%args47820$anf_45bind40244$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40617, %struct.ScmObj* %args47820$anf_45bind40244$1)
store volatile %struct.ScmObj* %args47820$anf_45bind40244$2, %struct.ScmObj** %stackaddr$prim49851, align 8
%clofunc49852 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40244)
musttail call tailcc void %clofunc49852(%struct.ScmObj* %anf_45bind40244, %struct.ScmObj* %args47820$anf_45bind40244$2)
ret void
}

define tailcc void @proc_clo$ae40617(%struct.ScmObj* %env$ae40617,%struct.ScmObj* %current_45args47817) {
%stackaddr$env-ref49853 = alloca %struct.ScmObj*, align 8
%k40582 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40617, i64 0)
store %struct.ScmObj* %k40582, %struct.ScmObj** %stackaddr$env-ref49853
%stackaddr$env-ref49854 = alloca %struct.ScmObj*, align 8
%args40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40617, i64 1)
store %struct.ScmObj* %args40107, %struct.ScmObj** %stackaddr$env-ref49854
%stackaddr$prim49855 = alloca %struct.ScmObj*, align 8
%_95k40584 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47817)
store volatile %struct.ScmObj* %_95k40584, %struct.ScmObj** %stackaddr$prim49855, align 8
%stackaddr$prim49856 = alloca %struct.ScmObj*, align 8
%current_45args47818 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47817)
store volatile %struct.ScmObj* %current_45args47818, %struct.ScmObj** %stackaddr$prim49856, align 8
%stackaddr$prim49857 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47818)
store volatile %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$prim49857, align 8
%stackaddr$prim49858 = alloca %struct.ScmObj*, align 8
%cpsargs40585 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40582, %struct.ScmObj* %args40107)
store volatile %struct.ScmObj* %cpsargs40585, %struct.ScmObj** %stackaddr$prim49858, align 8
%clofunc49859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40245)
musttail call tailcc void %clofunc49859(%struct.ScmObj* %anf_45bind40245, %struct.ScmObj* %cpsargs40585)
ret void
}

define tailcc void @proc_clo$ae40589(%struct.ScmObj* %env$ae40589,%struct.ScmObj* %current_45args47825) {
%stackaddr$prim49860 = alloca %struct.ScmObj*, align 8
%k40586 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47825)
store volatile %struct.ScmObj* %k40586, %struct.ScmObj** %stackaddr$prim49860, align 8
%stackaddr$prim49861 = alloca %struct.ScmObj*, align 8
%current_45args47826 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47825)
store volatile %struct.ScmObj* %current_45args47826, %struct.ScmObj** %stackaddr$prim49861, align 8
%stackaddr$prim49862 = alloca %struct.ScmObj*, align 8
%yu40104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47826)
store volatile %struct.ScmObj* %yu40104, %struct.ScmObj** %stackaddr$prim49862, align 8
%args47828$yu40104$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49863 = alloca %struct.ScmObj*, align 8
%args47828$yu40104$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40104, %struct.ScmObj* %args47828$yu40104$0)
store volatile %struct.ScmObj* %args47828$yu40104$1, %struct.ScmObj** %stackaddr$prim49863, align 8
%stackaddr$prim49864 = alloca %struct.ScmObj*, align 8
%args47828$yu40104$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40586, %struct.ScmObj* %args47828$yu40104$1)
store volatile %struct.ScmObj* %args47828$yu40104$2, %struct.ScmObj** %stackaddr$prim49864, align 8
%clofunc49865 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40104)
musttail call tailcc void %clofunc49865(%struct.ScmObj* %yu40104, %struct.ScmObj* %args47828$yu40104$2)
ret void
}