#lang racket

(provide symbol-append prims-list prim? reserved? datum?
         c-name prim-name applyprim-name simplify-core
         ; evaluation helper functions
         eval-scheme eval-core eval-proc eval-llvm run-llvm
         ; predicates to determine grammatical correctness.
         scheme-exp? core-exp? assignment-converted-exp? alphatized-exp?
         anf-exp? cps-exp? single-proc? proc-exp? llvm-sexpr?
         ; tester functions to help test for correct functionality.
         test-desugar test-alphatize test-anf-convert test-cps-convert
         test-closure-convert test-proc->llvm)

(define/contract (symbol-append s . ss)
  (->* (symbol?) #:rest (listof symbol?) symbol?)
  (match ss
    ['() s]
    [(cons _ _) (string->symbol (string-append (~a s) (~a (apply symbol-append ss))))]))


;;;;; DONT TOUCH THIS PLACE!!!!!!
(define libgc-path "/usr/local/lib/libgc.a")
(define gc-include-path "~/Documents/bdwgc/include/")
(define clang_bin "clang++")

(define prims-list
  '(= > < <= >= + - * /
      cons? null? cons car cdr list first second third fourth fifth list
      length list-tail drop take member memv map append foldl foldr
      vector? vector make-vector vector-ref vector-set! vector-length
      set set->list list->set set-add set-union set-count set-first set-rest set-remove
      hash hash-ref hash-set hash-count hash-keys hash-has-key? hash?
      list? void? promise? procedure? number? integer?
      error void print display write exit halt eq? eqv? equal? not))

(define (prim? op)
  (if (member op prims-list)
      #t
      #f))

; the list of reserved special forms
(define reserved-list '(letrec letrec* let let* if and or set! quote begin
                         cond case when unless delay force dynamic-wind
                         raise guard call/cc prim apply-prim))
(define (reserved? id) (member id reserved-list))

; converts a symbol to a symbol that would be a legal variable name in the C family of languages.
(define/contract (c-name s)
  (-> symbol? symbol?)
  (define ok-set
    (list->set (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789$")))
  (string->symbol (foldr string-append
                         ""
                         (map (lambda (c)
                                (if (set-member? ok-set c)
                                    (string c)
                                    (string-append "_" (number->string (char->integer c)))))
                              (string->list (symbol->string s))))))

; converts a symbol (which represents a primitive)
; to the corresponding function name in the header.
(define (prim-name op)
  (symbol-append 'prim_ (c-name op)))
; same as prim-name, but for `apply prim` expressions
(define (applyprim-name op)
  (symbol-append 'applyprim_ (c-name op)))

;;;;;;;;;;;;;;;;;; Predicates for grammar conformity ;;;;;;;;;;;;;;;;;;

(define (datum? d)
  (match d
    [`#(,(? datum?) ...) #t]
    [`(,(? datum?) ...) #t]
    [(cons datum? datum?) #t]
    [(? string?) #t]
    [(? integer?) #t]
    [(? symbol?) #t]
    [(? boolean?) #t]
    [else (pretty-print `(bad-datum ,d)) #f]))


(define (scheme-exp? e [env (set)])
  (define (var? x) (symbol? x))
  (define ((rec/with env) e)
    (scheme-exp? e env))
  (define (no-duplicates? lst)
    (if (= (set-count (list->set lst)) (length lst))
        #t
        (begin (pretty-print `(duplicate vars ,lst))
               #f)))
  (define (ext env lst) (set-union env (list->set lst)))
  (define (cond-clause? cls)
    (match cls
      [`(else ,(? rec/with env)) #t]
      [`(,(? (rec/with env))) #t]
      [`(,(? (rec/with env)) ,(? (rec/with env))) #t]
      [else #f]))
  (define (case-clause? cls)
    (match cls
      [`((,(? datum?) ...) ,(? (rec/with env))) #t]
      [else #f]))
  (match e
    [`(letrec* ([,(? var? xs) ,es] ...) ,e0)
     (and (no-duplicates? xs)
          (andmap (rec/with (ext env xs))
                  (cons e0 es)))]
    [`(letrec ([,(? var? xs) ,es] ...) ,e0)
     (and (no-duplicates? xs)
          (andmap (rec/with (ext env xs))
                  (cons e0 es)))]
    [`(let* () ,e0)
     ((rec/with env) e0)]
    [`(let* ([,x ,e0]) ,e1)
     ((rec/with env) `(let ([,x ,e0]) ,e1))]
    [`(let* ([,x ,e0] . ,rest) ,e1)
     ((rec/with env) `(let ([,x ,e0]) (let* ,rest ,e1)))]
    [`(let ([,(? symbol? xs) ,(? (rec/with env) es)] ...) ,e0)
     (and (no-duplicates? xs)
          ((rec/with (ext env xs)) e0))]
    [`(let ,(? var? lp)  ([,xs ,es] ...) ,e0)
     ((rec/with env) `(letrec ([,lp (lambda ,xs ,e0)]) (,lp . ,es)))]
    [`(lambda (,(? var? xs) ...) ,e0)
     (and (no-duplicates? xs)
          ((rec/with (ext env xs)) e0))]
    [`(lambda ,(? var? x) ,e0)
     ((rec/with (ext env (list x))) e0)]
    [`(lambda (,(? var? x0) ,(? var? xs) ... . ,(? var? improper-arg)) ,e0)
     (and (no-duplicates? `(,x0 ,@xs ,improper-arg))
          ((rec/with (ext env `(,x0 ,@xs ,improper-arg))) e0))]
    [`(delay ,(? (rec/with env))) #t]
    [`(force ,(? (rec/with env))) #t]
    [`(guard (,(? symbol? x) ,clauses ...) ,(? (rec/with env)))
     (scheme-exp? `(cond . ,clauses) (ext env (list x)))]
    [`(raise ,(? (rec/with env))) #t]
    [`(dynamic-wind ,(? (rec/with env)) ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`(cond ,(? cond-clause?) ... (else ,(? (rec/with env)))) #t]
    [`(cond ,(? cond-clause?) ...) #t]
    [`(case ,(? (rec/with env)) ,(? case-clause?) ...) #t]
    [`(case ,(? (rec/with env)) ,(? case-clause?) ... (else ,(? (rec/with env)))) #t]
    [`(and ,(? (rec/with env)) ...) #t]
    [`(or ,(? (rec/with env)) ...) #t]
    [`(when ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`(unless ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`(if ,(? (rec/with env)) ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`(set! ,(? symbol?) ,(? (rec/with env))) #t]
    [`(begin ,(? (rec/with env)) ,(? (rec/with env)) ...) #t]
    [`(call/cc ,(? (rec/with env))) #t]
    [`(let/cc ,(? symbol? x) ,eb) ((rec/with (ext env (list x))) eb)]
    [(? var? x) (if (or (set-member? env x) (prim? x))
                    #t
                    (begin (pretty-print `(unbound-var: ,x)) #f))]
    [`(quote ,(? datum?)) #t]
    [`(,(? prim?) ,(? (rec/with env)) ...) #t]
    [`(apply ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`#(,(? (rec/with env))) (displayln 'invec) #t]
    [`(,(? (rec/with env)) ,(? (rec/with env)) ...) #t]
    [else (pretty-print `(bad-scheme ,e ,env)) #f]))

(define (core-exp? e [env (set)])
  (define (var? x) (symbol? x))
  (define ((rec/with env) e)
    (core-exp? e env))
  (define (no-duplicates? lst)
    (= (set-count (list->set lst)) (length lst)))
  (define (ext env lst)
    (set-union env (list->set lst)))
  (match e
    [`(let ([,(? var? xs) ,(? (rec/with env) es)] ...) ,e0)
     (and (no-duplicates? xs)
          ((rec/with (ext env xs)) e0))]
    [`(lambda (,(? var? xs) ...) ,e0)
     (and (no-duplicates? xs)
          ((rec/with (ext env xs)) e0))]
    [`(lambda ,(? var? x) ,e0)
     ((rec/with (ext env (list x))) e0)]
    [`(apply ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`(if ,(? (rec/with env)) ,(? (rec/with env)) ,(? (rec/with env))) #t]
    [`(set! ,(? symbol?) ,(? (rec/with env))) #t]
    [`(call/cc ,(? (rec/with env))) #t]
    [(? var? x) (set-member? env x)]
    [`(quote ,(? datum?)) #t]
    [`(prim ,(? prim?) ,(? (rec/with env)) ...) #t]
    [`(apply-prim ,(? prim?) ,(? (rec/with env))) #t]
    [`(,(? (rec/with env)) ,(? (rec/with env)) ...) #t]
    [else (pretty-print `(bad-core ,e ,env)) #f]))

(define (assignment-converted-exp? e)
  (define (no-set!? e)
    (match e
      [`(set! ,_ ,_) #f]
      [`(let ([,_ ,(? no-set!?)] ...) ,(? no-set!?)) #t]
      [`(lambda ,_ ,(? no-set!?)) #t]
      [`(apply ,(? no-set!?) ,(? no-set!?)) #t]
      [`(prim ,_ ,(? no-set!?) ...) #t]
      [`(apply-prim ,_ ,(? no-set!?)) #t]
      [`(if ,(? no-set!?) ,(? no-set!?) ,(? no-set!?)) #t]
      [`(call/cc ,(? no-set!?)) #t]
      [(or (? symbol?) `(quote ,(? datum?))) #t]
      [`(,(? no-set!?) ...) #t]
      [else #t]))
  ; an expression is assignment converted if it simply does not use set!
  (and (core-exp? e) (no-set!? e)))

(define (alphatized-exp? e)
  (define seen (set))
  (define (not-seen-var? x)
    (define valid (and (var? x)  (not (set-member? seen x))))
    (set! seen (set-add seen x))
    valid)
  (define (var? x) (symbol? x))
  (define (no-duplicates? lst)
    (= (set-count (list->set lst)) (length lst)))
  (define (ext env lst)
    (set-union env (list->set lst)))
  (define (alpha? e)
    (match e
      [`(let ([,(? not-seen-var? xs) ,(? alpha? es)] ...) ,e0)
       (and (no-duplicates? xs)
            (alpha? e0))]
      [`(lambda (,(? not-seen-var? xs) ...) ,e0)
       (and (no-duplicates? xs)
            (alpha? e0))]
      [`(lambda ,(? not-seen-var? x) ,e0)
       (alpha? e0)]
      [`(apply ,(? alpha?) ,(? alpha?)) #t]
      [`(if ,(? alpha?) ,(? alpha?) ,(? alpha?)) #t]
      [`(call/cc ,(? alpha?)) #t]
      [(? var? x) #t]
      [`(quote ,(? datum?)) #t]
      [`(prim ,(? prim?) ,(? alpha?) ...) #t]
      [`(apply-prim ,(? prim?) ,(? alpha?)) #t]
      [`(,(? (and/c (not/c (lambda (x)
                             (member x '(let lambda apply if call/cc quote prim quote-prim))))
                    alpha?))
         ,(? alpha?) ...) #t]
      [else (pretty-print `(bad-alphatized ,e)) #f]))
  (and (core-exp? e) (alpha? e)))


(define (anf-exp? e)
  (define (a-exp? e)
    (match e
      [`(lambda ,xs ,(? c-exp? e0)) #t]
      [`',dat #t]
      [(? symbol? x) #t]
      [else #f]))
  (define (c-exp? e)
    (match e
      [`(let ([,(? symbol? x) ,(? c-exp? rhs)]) ,(? c-exp? e0)) #t]
      [`(if ,(? a-exp? ae) ,(? c-exp? e0) ,(? c-exp? e1)) #t]
      [`(prim ,op ,(? a-exp? aes) ...) #t]
      [`(apply-prim ,op ,(? a-exp? ae)) #t]
      [`(call/cc ,(? a-exp? ae)) #t]
      [`(apply ,(? a-exp? aes) ,(? a-exp? aes)) #t]
      [`(,(? (and/c (not/c reserved?) a-exp?) aef) ,(? a-exp? aes) ...) #t]
      [(? a-exp? e) #t]
      [else (pretty-print `(bad-anf ,e)) #f]))
  (and (core-exp? e) (c-exp? e)))

(define (cps-exp? e)
  (define (a-exp? e)
    (match e
      [`(lambda ,xs ,(? c-exp? e0)) #t]
      [`',dat #t]
      [(? symbol? x) #t]
      [else (pretty-print `(bad-cps-ae ,e)) #f]))
  (define (c-exp? e)
    (match e
      [`(let ([,(? symbol? x) (prim ,op ,(? a-exp? aes) ...)]) ,(? c-exp? e0)) #t]
      [`(let ([,(? symbol? x) (apply-prim ,op ,(? a-exp? ae))]) ,(? c-exp? e0)) #t]
      [`(let ([,(? symbol? x) (lambda ,xs ,(? c-exp? lam-e))]) ,(? c-exp? e0)) #t]
      [`(let ([,(? symbol? x) ',dat]) ,(? c-exp? e0)) #t]
      [`(if ,(? a-exp? ae) ,(? c-exp? e0) ,(? c-exp? e1)) #t]
      [`(apply ,(? a-exp? aes) ,(? a-exp? aes)) #t]
      [`(,(? (and/c (not/c reserved?) a-exp?) aef) ,(? a-exp? aes) ...) #t]
      [else (pretty-print `(bad-cps-e ,e)) #f]))
  (and (anf-exp? e) (c-exp? e)))

(define (single-proc? e)
  (define (c-exp? e)
    (match e
      [`(let ([,(? symbol?) (make-closure ,(? symbol?) ...)]) ,(? c-exp?)) #t]
      [`(let ([,(? symbol?) (env-ref ,(? symbol?) ,(? integer?))]) ,(? c-exp?)) #t]
      [`(let ([,(? symbol?) (prim ,_ ,(? symbol? xs) ...)]) ,(? c-exp?)) #t]
      [`(let ([,(? symbol?) (apply-prim ,_ ,(? symbol?))]) ,(? c-exp?)) #t]
      [`(let ([,(? symbol?) ',_]) ,(? c-exp?)) #t]
      [`(if ,(? symbol?) ,(? c-exp?) ,(? c-exp?)) #t]
      [`(clo-app ,(? symbol?) ,(? symbol?) ...) #t]
      [else (pretty-print `(bad-proc-e ,e)) #f]))
  (match e
    [`(proc (,(? symbol?) ...) ,(? c-exp?)) #t]
    [else (pretty-print `(bad-proc ,e)) #f]))

(define (proc-exp? e)
  (and ((listof single-proc?) e)
       (match (first e)
         [`(proc (main ,_ ,_) ,_) #t]
         [_ #f])))

; TODO!
(define llvm-sexpr? identity)

;;;;;;;;;;;;;;;;;; evaluating code ;;;;;;;;;;;;;;;;;;

; used to evaluate non-desugared scheme code.
(define (eval-scheme e)
  (if (scheme-exp? e)
      (racket-ir-eval e)
      (error 'malformed-scheme)))


; this can be used to evaluate any lisp-based IR.
(define (eval-core e)
  (if (core-exp? e)
      (racket-ir-eval e)
      (error 'malformed-core)))

(define (eval-proc proc)
  (if (proc-exp? proc)
      (racket-proc-eval proc)
      (error 'malformed-proc-ir)))

;; Evaluates the given LLVM code.
;; Starts by compiling the header.cpp file into LLVM bytecodes,
;; and combining it with the input string.
;; then the combined file is compiled into an executable and ran.
(define (eval-llvm llvm-str [header-loc "."])
  ; freshly compile the header / runtime library if not already
  ;; use this line instead if you add garbage collection
  ;(system (string-append "clang++-10 header.cpp " " -I " gc-include-path " -S -emit-llvm -o header.ll"))
  (pretty-print "Hello")
  (define cpp-loc (string-append header-loc "/header.cpp"))
  (define ll-loc "./header.ll")
  (define combined-loc "./combined.ll")
  (define bin-loc "./bin")
  (system (string-append clang_bin " -std=c++17 " cpp-loc " -S -emit-llvm -o" ll-loc))
  (collect-garbage)
  (define header-str (read-string 500000000 (open-input-file ll-loc #:mode 'text)))
  (define llvm (string-append header-str "\n\n;;;header ended;;;\n\n" llvm-str))
  (with-output-to-file combined-loc (λ () (display llvm)) #:exists 'replace)
)

(define (run-llvm llvm-str [header-loc "."])
  ; freshly compile the header / runtime library if not already
  ;; use this line instead if you add garbage collection
  ;(system (string-append "clang++-10 header.cpp " " -I " gc-include-path " -S -emit-llvm -o header.ll"))
  (define cpp-loc (string-append header-loc "/header.cpp"))
  (define ll-loc "./header.ll")
  (define combined-loc "./combined.ll")
  (define bin-loc "./bin")
  (system (string-append clang_bin " " combined-loc " -o " bin-loc))
  (collect-garbage)
  (match-define `(,out-port ,in-port ,id ,err-port ,callback) (process bin-loc))
  (define starttime (current-milliseconds))
  (let loop ()
    (define time (current-milliseconds))
    (define status (callback 'status))
    (if (> time (+ starttime 20000))
        (begin (pretty-print '(eval-llvm "binary execution timed out")) (void))
        (if (eq? status 'done-ok)
            ; use a read to turn the printed value back into a racket value
            (let* ([output (read out-port)]
                   [v (eval output (make-base-namespace))])
              (callback 'kill)
              v)
            (if (eq? status 'done-error)
                (begin (pretty-print '(eval-llvm: bad-status-code)) (void))
                (loop))))))



; this is be used to interpret IRs.
(define (racket-ir-eval e)
  (with-handlers ([exn:fail? (lambda (x)
                               (displayln "Evaluation failed:")
                               (pretty-print x)
                               (error 'eval-ir-fail))])
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'rnrs)
      (namespace-require 'racket)
      (namespace-require 'srfi/34)
      (eval (compile
             `(call/cc (lambda (exit+)
                         (define (halt x) (exit+ x))
                         (define (prim op . args) (apply op args))
                         (define (apply-prim op args) (apply op args))
                         ,e)))))))

; This can run programs that correspond to proc-exp?
(define (racket-proc-eval procs)
  (with-handlers ([exn:fail? (lambda (x)
                               (pretty-print "Evaluation failed:")
                               (pretty-print x)
                               (error 'eval-proc-fail))])
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'rnrs)
      (namespace-require 'racket)
      (namespace-require 'srfi/34)
      (eval (compile
             `(begin
                (call/cc (lambda (exit+)
                           (define (halt x) (exit+ x))
                           (define datum (lambda (d) d))
                           (define (prim op . args) (apply op args))
                           (define (apply-prim op args) (apply op args))
                           (define (procedure? p)
                             (and (vector? p) (equal? '%clo (vector-ref p (- (vector-length p) 1)))))
                           (define (make-closure . args) (list->vector (append args '(%clo))))
                           ; when interpreting procs, the 0th vector element is the closure pointer
                           ; so we need to offset by 1.
                           (define (env-ref clo n) (vector-ref clo (+ n 1)))
                           (define (clo-app f . vs) (apply (vector-ref f 0) (cons f vs)))
                           ,@(map (match-lambda [`(proc (,xs ...) ,bdy) `(define ,xs ,bdy)]) procs)
                           (main '0 '0)))))))))

;;;;;;;;;;;;;;;;;; local testing helpers ;;;;;;;;;;;;;;;;;;

; This function differs from previous `test-desugar` functions.
; it integrates the `simplify-core` function to remove many
; primitives, simplifying the header code.
(define/contract (test-desugar desugar scheme-prog)
  (-> (-> scheme-exp? core-exp?) scheme-exp? boolean?)
  (define expected (eval-scheme scheme-prog))
  (define core-e (simplify-core (desugar scheme-prog)))
  (define got (eval-core core-e))
  (if (equal? expected got)
      #t
      (begin
        (if (core-exp? core-e)
            (displayln (format "Test-desugar: different values. Expected: ~a, Got: ~a" expected got))
            (displayln "Output not in core language."))
        #f)))


; tests boxing + alphatizing
(define/contract (test-alphatize box rename prog)
  (-> (-> core-exp? assignment-converted-exp?)
      (-> assignment-converted-exp? alphatized-exp?) core-exp? boolean?)
  (define expected-val (eval-core prog))
  (define alphatized-e (rename (box prog)))
  (define alphatized-val (eval-core alphatized-e))
  (if (equal? expected-val alphatized-val)
      #t
      (begin
        (if (alphatized-exp? alphatized-e)
            (displayln (format (string-append "Test-alphatized: Output from boxing and alphatizing "
                                              "does not match the expected value.\n"
                                              "Expected: ~a, Got: ~a")
                               expected-val alphatized-val))
            (displayln "Output not in alphatized language."))
        #f)))


(define/contract (test-anf-convert anf-convert prog)
  (-> (-> alphatized-exp? anf-exp?) alphatized-exp? boolean?)
  (define expected-val (eval-core prog))
  (define anf-e (anf-convert prog))
  (define anf-val (eval-core anf-e))
  (if (equal? expected-val anf-val)
      #t
      (begin
        (if (anf-exp? anf-e)
            (display (format (string-append "Test-anf-convert: Output from ANF Conversion "
                                            "does not match the expected value.\n"
                                            "Expected: ~a, Got: ~a")
                             expected-val anf-val))
            (displayln "Output not in ANF language."))
        #f)))

(define/contract (test-cps-convert cps-convert prog)
  (-> (-> anf-exp? cps-exp?) anf-exp? boolean?)
  (define expected-val (eval-core prog))
  (define cps-e (cps-convert prog))
  (define cps-val (eval-core cps-e))
  (if (equal? expected-val cps-val)
      #t
      (begin
        (if (cps-exp? cps-e)
            (displayln (format (string-append "Test-cps-convert: Output from CPS Conversion "
                                              "does not match the expected value.\n"
                                              "Expected: ~a, Got: ~a")
                               expected-val cps-val))
            (displayln "Output not in CPS language."))
        #f)))


(define/contract (test-closure-convert closure-convert prog)
  (-> (-> cps-exp? proc-exp?) cps-exp? boolean?)
  (define expected-val (eval-core prog))
  (define proc-e (closure-convert prog))
  (define proc-val (eval-proc proc-e))
  (if (equal? expected-val proc-val)
      #t
      (begin
        (if (proc-exp? proc-e)
            (displayln (format (string-append "Test-closure-convert: Output from closure conversion "
                                              "does not match the expected value.\n"
                                              "Expected: ~a, Got: ~a")
                               expected-val proc-val))
            (displayln "Output not in Proc language."))
        #f)))

; TODO: make the proc->llvm return a llvm-exp?
(define/contract (test-proc->llvm proc->llvm prog)
  (-> (-> proc-exp? string?) proc-exp? boolean?)
  (define expected-val (eval-proc prog))
  (define llvm (proc->llvm prog))
  (define llvm-val (eval-llvm llvm))
  (if (equal? expected-val llvm-val)
      #t
      (begin
        (displayln (format (string-append "Test-proc->llvm: Output from LLVM Conversion "
                                          "does not match the expected value.\n"
                                          "Expected: ~a, Got: ~a")
                           expected-val llvm-val))
        #f)))


; Remove many primitives and simplify certain instances of primitives.
; Some are removed simply by 'short circuiting' i.e. (prim +) => 0.
; Others are added as functions and mapped to them internally.
(define/contract (simplify-core core-e)
  (-> core-exp? core-exp?)
  (define (T e)
    (match e
      [`(let ([,xs ,es] ...) ,e0) `(let ,(map list xs (map T es)) ,(T e0))]
      [`(lambda ,xs ,e0) `(lambda ,xs ,(T e0))]
      [`(apply ,e0 ,e1) `(apply ,(T e0) ,(T e1))]
      [`(if ,e0 ,e1 ,e2) `(if ,(T e0) ,(T e1) ,(T e2))]
      [`(set! ,x ,e0) `(set! ,x ,(T e0))]
      [`(call/cc ,e0) `(call/cc ,(T e0))]
      [(? symbol? x) x]
      [`(quote ,(? symbol? x)) `(quote ,x)]
      [`(quote #(,ds ...)) (T `(prim vector ,@(map (lambda (d) `',d) ds)))]
      [`(quote ,(? list? lst)) (T `(prim list ,@(map (lambda (d) `',d) lst)))]
      [`(quote ,other) `(quote ,other)]
      [`(prim +) ''0]
      [`(prim + ,e0) (T e0)]
      [`(prim + ,e0 ,e1 ,e2 ,es ...) (T `(prim + ,e0 (prim + ,e1 ,e2 ,@es)))]
      [`(prim *) ''1]
      [`(prim * ,e0) (T e0)]
      [`(prim * ,e0 ,e1 ,e2 ,es ...) (T `(prim * ,e0 (prim * ,e1 ,e2 ,@es)))]
      [`(prim - ,e0) (T `(prim * ,e0 -1))]
      [`(prim - ,e0 ,es ...)
       #:when (> (length es) 1)
       (T `(prim - (prim - ,e0 ,@(drop-right es 1)) ,(last es)))]
      [`(prim / ,e0 ,e1) `(prim / ,(T e0) ,(T e1))]
      ; Remove list, vector->apply vector, map foldl, foldr, drop, memv, >, >=, ...
      [`(prim list ,es ...) `((lambda lst lst) ,@(map T es))]
      [`(apply-prim list ,e0) (T e0)]
      [`(prim vector ,es ...) (T `((lambda el (apply-prim vector el)) ,@es))]
      [`(prim foldl ,e0 ,e1 ,e2) `(%foldl1 ,@(map T (list e0 e1 e2)))]
      [`(prim foldr ,e0 ,e1 ,e2) `(%foldr1 ,@(map T (list e0 e1 e2)))]
      [`(prim map ,e0 ,e1) `(%map1 ,@(map T (list e0 e1)))]
      [`(prim ,op ,es ...)
       #:when (member op '(drop memv / > >= list? drop-right length append last
                                map foldl foldr first second third fourth))
       `(,(string->symbol (string-append "%" (symbol->string op))) ,@(map T es))]
      [`(apply-prim ,op ,e0)
       #:when (member op '(drop memv / > >= list? drop-right length append last
                                map foldl foldr first second third fourth))
       `(apply ,(string->symbol (string-append "%" (symbol->string op))) ,(T e0))]

      [`(prim ,op ,es ...)
       `(prim ,op ,@(map T es))]
      [`(apply-prim ,op ,e0)
       `(apply-prim ,op ,(T e0))]
      [`(,ef ,es ...)
       `(,(T ef) ,@(map T es))]))
  ; wrap input program in these helper functions such as the Y-combinator (for recursion).
  `(let ([Ycmb
          ((lambda (yu) (yu yu))
           (lambda (y) (lambda (f) (f (lambda args (apply ((y y) f) args))))))])
     (let ([%foldr1
            (Ycmb
             (lambda (%foldr1)
               (lambda (f acc lst)
                 (if (prim null? lst)
                     acc
                     (f (prim car lst)
                        (%foldr1 f acc (prim cdr lst)))))))]
           [%map1 (Ycmb (lambda (%map) (lambda (f lst) (if (prim null? lst)
                                                           '()
                                                           (prim cons (f (prim car lst))
                                                                 (%map f (prim cdr lst)))))))]
           [%take
            (Ycmb (lambda (%take) (lambda (lst n) (if (prim = n '0)
                                                      '()
                                                      (if (prim null? lst) '()
                                                          (prim cons (prim car lst)
                                                                (%take (prim cdr lst)
                                                                       (prim - n '1))))))))]
           [%length (Ycmb (lambda (%length) (lambda (lst)
                                              (if (prim null? lst)
                                                  '0
                                                  (prim + '1 (%length (prim cdr lst)))))))]
           [%foldl1
            (Ycmb
             (lambda (%foldl1)
               (lambda (f acc lst)
                 (if (prim null? lst)
                     acc
                     (%foldl1 f (f (prim car lst) acc) (prim cdr lst))))))])
       (let ([%last (lambda (lst) (%foldl1 (lambda (x y) x) '() lst))]
             [%drop-right (lambda (lst n) (%take lst (prim - (%length lst) n)))]
             [%foldr
              (Ycmb
               (lambda (%foldr)
                 (lambda args
                   (let ([f (prim car args)]
                         [acc (prim car (prim cdr args))]
                         [lsts (prim cdr (prim cdr args))])
                     (if (%foldr1 (lambda (lst b) (if b b (prim null? lst))) '#f lsts)
                         acc
                         (let ([lsts+ (%map1 (lambda (x) (prim cdr x)) lsts)]
                               [vs (%map1 (lambda (x) (prim car x)) lsts)])
                           (apply f (%foldr1 (lambda (a b) (prim cons a b))
                                             (prim cons
                                                   (apply %foldr (prim cons f (prim cons acc lsts+)))
                                                   '())
                                             vs))))))))])
         (let ([%map1
                (lambda (f lst)
                  (%foldr1 (lambda (v r) (prim cons (f v) r)) '() lst))]
               [%map
                (lambda args
                  (let ([f (prim car args)]
                        [lsts (prim cdr args)])
                    (apply %foldr (prim cons
                                        (lambda fargs
                                          (prim cons
                                                (apply f (%drop-right fargs '1))
                                                (%last fargs)))
                                        (prim cons '() lsts)))))])
           (let ([%foldl
                  (Ycmb
                   (lambda (%foldl)
                     (lambda args
                       (let ([f (prim car args)]
                             [acc (prim car (prim cdr args))]
                             [lsts (prim cdr (prim cdr args))])
                         (if (%foldr1 (lambda (lst b) (if b b (prim null? lst))) '#f lsts)
                             acc
                             (let ([lsts+ (%map1 (lambda (x) (prim cdr x)) lsts)]
                                   [vs (%map1 (lambda (x) (prim car x)) lsts)])
                               (let ([acc+ (apply f (%foldr (lambda (a b) (prim cons a b))
                                                            (prim cons acc '()) vs))])
                                 (apply %foldl (prim cons f (prim cons acc+ lsts+))))))))))]
                 [%>
                  (lambda (a b) ; we'll assume comparators are binary, but we could tweak this here
                    (prim not (prim <= a b)))]
                 [%>=
                  (lambda (a b)
                    (prim not (prim < a b)))]
                 [%append (let ([%append '()])
                            (let ([_0 (set! %append (lambda (ls0 ls1)
                                                      (if (prim null? ls0)
                                                          ls1
                                                          (prim cons (prim car ls0)
                                                                (%append (prim cdr ls0) ls1)))))])
                              %append))]
                 [%list?
                  (lambda (a)
                    (let ([cc (call/cc (lambda (k) k))])
                      (if (prim null? a)
                          '#t
                          (if (prim cons? a)
                              (let ([b (prim cdr a)])
                                (let ([_0 (set! a (prim cdr a))])
                                  (cc cc)))
                              '#f))))]
                 [%drop
                  (lambda (lst n)
                    (let ([cc (call/cc (lambda (u) (u u)))])
                      (if (prim = '0 n)
                          lst
                          (let ([_0 (set! lst (prim cdr lst))]
                                [_1 (set! n (prim - n '1))])
                            (cc cc)))))]
                 [%memv
                  (lambda (v lst)
                    (let ([cc (call/cc (lambda (u) (u u)))])
                      (if (prim null? lst)
                          '#f
                          (if (prim eqv? (prim car lst) v)
                              lst
                              (let ([_0 (set! lst (prim cdr lst))])
                                (cc cc))))))]
                 [%/ (lambda args (if (prim null? args) '1
                                      (if (prim null? (prim cdr args))
                                          (prim car args)
                                          (%foldl1 (lambda (n v) (prim / v n))
                                                   (prim car args) (prim cdr args)))))]
                 [%first (lambda (x) (prim car x))]
                 [%second (lambda (x) (prim car (prim cdr x)))]
                 [%third (lambda (x) (prim car (prim cdr (prim cdr x))))]
                 [%fourth (lambda (x) (prim car (prim cdr (prim cdr (prim cdr x)))))])
             ,(T core-e)))))))



