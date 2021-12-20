##Yixing's Scheme Compiler

### Introduction:
This is final project for CIS400/600 combined class for Intro. to Compiler Construction  in Fall 2021 at Syracuse University.
This Scheme Compiler is written in nano-pass style and has following passes:
+ Desugar: Desugars the output of top level into a smaller core language. Desugars things like letrec*/letrec, guard/raise, promises, cond, case, etc...


+ Simplify IR: Simplifies the intermediate representation produced by desugar and performs some optimizations, to make future passes easier. For example, (+) => 0, (+ 2) => 2, (+ 1 2 3 4) => (+ 1 (+ 2 (+ 3 4)).


+ Assignment Convert: Removes set! mutations by boxing all mutable variables inside of a size 1 vector.


+ Alphatize: Makes all binding names unique, removing shadowing.


+ ANF Convert: Flattens let forms with multiple bindings into nested lets. Removes all subexpressions by hoisting them to let bindings. This pass, combined with Alphatize, and Assignment Convert, give us an IR that is in Static Single Assigment (SSA) forms, in preparation for later passes.


+ CPS Convert: Converts our IR to continuation passing style, where no function call returns, and instead the current continuation is invoked. This makes it so that points that would have extended the stack are now closures (with an environment for free variable references), and we pass forward callback functions that will be called in tail position, eliminating the need for a stack.


+ Closure Convert: Removes all lambdas, replaces them with closures, with appropriate environment references, lifts variable references to let bindings, and removes variadic lambdas - by having all lambda take in one parameter of a list of arguments, then extracting the original expected arguments from that list. The phase outputs a list of procedure abstracts, with a minimal language.


+ LLVM: With our IR now being in SSA, with continuation passing style, we can use the LLVM backend to compile our IR into executable bytecode. This pass takes the procedure output from the previous phase, and outputs strings of LLVM code that will produce an equivalent LLVM IR. This LLVM string is then concatenated to a LLVM compiled C++ header file (which contains some helper functions for things like primative operations and printing), and then compiled via clang++ into an executable bytecode file with default named bin.
---
### Quickstart:
```text
git clone https://github.com/Yixing-Chen-Shawn/Yixing-s-Scheme-Compiler.git
```
Install everything on your machine (not hard, I recommend you do this)


You will need the following:


+ Racket and the Racket command-line tools
https://docs.racket-lang.org/guide/cmdline-tools.html


+ LLVM and Clang
Essentially any version will work from the past five years. Install via brew on Mac (please install Brew if you have a mac but haven’t added it yet) and then do a brew install llvm (and probably clang, too)
https://embeddedartistry.com/blog/2017/02/24/installing-llvm-clang-on-osx/


+ If you’re using a Linux distribution there will almost certainly be a package (https://apt.llvm.org/):
E.g., apt-get install clang-format clang-tidy clang-tools clang clangd libc++-dev libc++1 libc++abi-dev libc++abi1 libclang-dev libclang1 liblldb-dev libllvm-ocaml-dev libomp-dev libomp5 lld lldb llvm-dev llvm-runtime llvm python-clang.


+ Make sure you have python3 installed.

Use terminal to navigate into the top directory level of the project, type
```text
python3 tester.py -av
```
Use command line below to perform specific single test (for example, perform public-case-1.scm test)
```text
python3 tester.py -t public-case-1.scm
```

There will be compiled llvm instruction produced in the combined.ll in each test folder. 

---
### Features not implemented:
+ Boehm Garbage Collector:
  Implementation of the Boehm Garbage Collector.


+ For / while / ... loops (e.g., implemented via call/cc).


+ Exceptions / raise / guard.


+ Other language forms, like strings and characters. 


