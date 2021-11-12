# Closure Conversion, LLVM Emission #

Your task on assignment 4 is broken up into two phases: closure
conversion and LLVM IR emission.

**NOTE**: this assignment requires a correct solution to projects p2
and p3, which you must include in `desugar.rkt` and `cps.rkt`. If you
are unsure of your solutions on p2/3 and would like to work on p4, you
may write me (Kris) and I will give you a solution key to p2/3, under
the condition that you do not subsequently turn it in as your p2/3
solution.

## Closure Conversion

The first phase runs output from assignment 3 through a function,
`closure-convert`, which expects inputs that satisfy `cps-exp?` and
should produce outputs that satisfy `proc-exp?`. This phase removes
all lambda abstractions and replaces them with new `make-closure` and
`env-ref` forms. Remaining atomic expressions other than variable
references are lifted to their own `let`-bindings. Finally, all
function calls must be non-variadic. This is done by turning all
fixed-arity functions into variadic functions and then all variadic
functions into unary functions that take an argument list explicitly.

As a reminder, this is the input language to this pass:

```
e ::= (let ([x (apply-prim op ae)]) e)
    | (let ([x (prim op ae ...)]) e)
    | (let ([x (lambda (x ...) e)]) e)
    | (let ([x (lambda x e)]) e)
    | (let ([x (quote dat)]) e)
    | (apply ae ae)
    | (ae ae ...)
    | (if ae e e)
ae ::= (lambda (x ...) e)
     | (lambda x e)
     | x
     | (quote dat)
dat is a datum satisfying datum? from utils.rkt
x is a variable (satisfies symbol?)
op is a symbol satisfying prim? from utils.rkt (if not already removed)
```

After this phase, the target should be in the `proc-exp?` language: a
list of first-order procedures that can be turned into LLVM functions
directly. The outermost program term should be encoded in a main
procedure `(proc (main) ...)`.

The output of this first pass will look like: 

```
p ::= ((proc (x x ...) e) ...)
e ::= (let ([x (apply-prim op x)]) e)
    | (let ([x (prim op x ...)]) e)
    | (let ([x (make-closure x x ...)]) e)
    | (let ([x (env-ref x nat)]) e)
    | (let ([x (quote dat)]) e)
    | (clo-app x x ...)
    | (if x e e)
dat is a datum satisfying datum? from utils.rkt
x is a variable (satisfies symbol?)
op is a symbol satisfying prim? from utils.rkt (if not already removed)
nat is a natural number satisfying natural? or integer?
```


## LLVM emission

**IMPORTANT**: for this project, we have built a language runtime,
implemented as `header.cpp`, which gets compiled alongside your final
LLVM code. You should be emitting LLVM code which calls functions in
`header.cpp`. Thus, before you can truly implement LLVM emission
correctly, we recommend you look over and understand `header.cpp`.

The second phase converts this first-order procedural language into
LLVM IR that can be combined with a runtime written in C to produce a
binary. You must implement proc->llvm so that it takes a program `p`
(a list of procedures) and produces a string encoding valid LLVM
IR. Applying `eval-llvm` on this string should return the correct
value.

This interpreter will make sure that the runtime, `header.cpp`, is
compiled to a file `header.ll` before it concatenates your llvm code
onto this compiled LLVM IR and saves the result to a file,
`combined.ll`. This file is then compiled and run using `clang++
combined.ll -o bin; ./bin`. If you run eval-llvm and do not see
correct output, you can investigate further by looking at the text of
`combined.ll` and trying to compile and run it manually. As always,
start with small examples and work your way up to larger ones.

The assignment 4 starter contains a file, `utils.rkt`, which provides
useful functions and a specification for the `proc-exp?` language, its
interpreter `eval-proc` and the LLVM compile-and-run routine
`eval-llvm`.

To reduce the scope of the final compiled code and resolve any
ambiguity in the assignment, you may continue to assume all inputs are
correct programs, and, for assignment 4, that all tests are public. If
you can pass every public test provided (files in tests/public/), you
should get 100% on the assignment. Each file is turned into a test
that starts `clo-` and one that starts `to-llvm-`; these test that
input through just closure conversion or through llvm respectively
(the latter requires that closure-conversion also work
properly). Tests that end in .scm are scheme inputs, tests that end in
.cps are cps-exp? inputs.

**NOTE**: we have provided starter code which you may choose to
use. Our starter code constructs an `llvm-sexpr` language which is
then translated to an LLVM string via the function
`llvm-sexpr->llvm-string`. To understand how `llvm-sexpr`s work,
please see the formatting function in `llvm-sexpr->llvm-string`, which
consumes `llvm-sexpr`s. We have also scaffolded out a function,
`llvm-sexpr-convert`, which converts a proc to an `llvm-sexpr`. You
essentially have two options: you may either (a) use our starter code
to emit `llvm-sexpr`s and use our rendering function
`llvm-sexpr->llvm-string` (the starter code is written to do this), or
(b) you may simply write out your own LLVM string and skip using our
starter code. This may or may not be simpler, but will have you
directly write LLVM code as the project output.

## Some unsorted advice for assignment 4

- You should make sure you have Clang and LLVM installed. Using the
standard version for your machine should be fine, we aren't doing
anything machine-specific in this project--I am using an M1 Macbook,
but Linux and Windows builds should also work.

- A helper function `simplify-ae` is included in the starter that may
  help you to simplify your CPS by lifting atomic expressions before
  bottom-up closure conversion. Some parts of this function may still
  need to be completed.

- You may notice that utils.rkt now inserts a new pass `simplify-ir`
  just after desugaring. This pass just adds some functions (such as
  length, foldl, map, append, etc), and simplifies how some primitive
  operations are used. You don’t need to modify this if you don’t want
  to.

- You do not need to modify `header.cpp`, but should look it over to
  see what kinds of primitives and functions you can use. You can call
  helper functions const_init_*, such as const_init_int(s32) to
  initialize constant datums, or inline the equivalent of these
  functions in your LLVM IR output.

- utils.rkt now provides `prim-name` and `applyprim-name` functions
  that turn a Racket primitive (i.e., a symbol like '+) into the
  corresponding C-equivalent names used by the runtime C library (in
  this case, `prim__43(u64, u64)` and `applyprim__43(u64)`; the former
  expects exactly 2 words of memory encoding two integers, the latter
  expects exactly one word of memory encoding a (pointer to a)
  cons-cell list of integers to sum).

- If you'd like, you may add arbitrary C code by using `clang++` to
  compile it from C/C++ (to LLVM) and then adding it to your
  `proc->llvm` output. This is also a good way to learn the LLVM IR by
  example. Try writing a simple `main.cpp` file and then running:
  `clang++ main.cpp -S -emit-llvm -o main.ll` so you can see its
  corresponding `.ll` output. You may wish to learn how to, e.g., call
  functions by doing this.


