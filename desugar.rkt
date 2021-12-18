#lang racket

; by First Last

(provide desugar desugar)
(require "utils.rkt")

;; TODO: replace with your `desugar` function
(define (desugar-aux e)
  (match e
    [(? prim? op) (if (member op '(+ - * /))
                      `(lambda args (apply-prim ,op args))
                      op)]
    [(? symbol? e) e]
    [`(quote ,dat) e]
    [`(letrec* ([,xs ,es] ...) ,ebody)
     `(let ,(foldr (lambda (x acc) (append `([,x (prim void)]) acc)) '() (desugar-aux xs))
        (let ,(foldr (lambda (x y acc) (append `([,(gensym 't) (set! ,x ,y)]) acc)) '() (desugar-aux xs) (desugar-aux es))
          ,(desugar-aux ebody)))]
    [`(letrec ([,xs ,es] ...) ,ebody)
     (desugar-aux `(let ,(foldr (lambda (x acc) (append `([,x (prim void)]) acc)) '() xs)
                     (let ,(foldr (lambda (x y acc) (append `([,(gensym 't) (set! ,x ,(match y
                                                                                        [(? prim? y)
                                                                                         (define cur (gensym 'x))
                                                                                         (desugar-aux `(lambda (,cur) (prim ,y ,cur)))]
                                                                                        [_ y]))]) acc)) '() xs es)
                       ,ebody)))]
    #|`(let ,(foldr (lambda (x acc) (append `([,x (prim void)]) acc)) '() (desugar-aux xs))
        (let ,(foldr (lambda (x y acc) (append `([,(gensym 't) (set! ,x ,y)]) acc)) '() (desugar-aux xs) (desugar-aux es))
          ,(desugar-aux ebody)))]|#
    [`(let* ([,xs ,es] ...) ,ebody)
     (define es-d (foldr (lambda (x acc) (cons (desugar-aux x) acc)) '() es))
     (foldr (lambda (x y acc) `(let ([,x ,y]) ,acc)) (desugar-aux ebody) xs es-d)]
    ;;translate let statement into lambdas
    ;;[`(let ([,xs ,es] ...) ,ebody) (desugar-aux (foldl (lambda (x y acc) `((lambda (,x) ,acc) ,y)) ebody xs es))]
    ;;translate something like (let ([x '1] [y '2]) x) into (let ([x '1]) (let ([y '2]) x))
    ;;[`(let ([,xs ,es] ...) ,ebody) (foldr (lambda (x y acc) `(let ([,x ,y]) ,acc)) (desugar-aux ebody) xs es)]
    [`(let ([,xs ,es] ...) ,ebody)
     `(let ,(map (lambda (x e) `(,x ,(desugar-aux e))) xs es) ,(desugar-aux ebody))]
    #|(define es-d (foldr (lambda (x acc) (cons (desugar-aux x) acc)) '() es))
     `(let ,(foldr (lambda (x y acc)
                     (append `([,x ,y]) acc)) '() xs es-d) ,(desugar-aux ebody))]|#
    ;;[`(let ([,var-name ,binding-expr]) ,ebody) `((lambda (,var-name) ,(desugar-aux binding-expr)) ,(desugar-aux ebody))]
    [`(let () ,ebody) (desugar-aux ebody)]
    [`(let ,loopvar ([,xs ,es] ...) ,ebody)
     (desugar-aux `(letrec ([,loopvar (lambda ,xs ,ebody)])
                     (,loopvar ,@es)))]
    [`(lambda () ,ebody) `(lambda (_) ,(desugar-aux ebody))]
    [`(lambda (,x) ,ebody) `(lambda (,x) ,(desugar-aux ebody))]
    ;;[`(lambda (,xs ...) ,ebody) `(lambda (,(first xs)) ,(desugar-aux `(lambda ,(rest xs) ,ebody)))]
    [`(lambda (,xs ...) ,ebody) `(lambda ,(foldr (lambda (x acc)
                                                   (append `(,x) acc)) '() (desugar-aux xs)) ,(desugar-aux ebody))]
    [`(lambda ,(? symbol? x) ,ebody)
     `(lambda ,x ,(desugar-aux ebody))]
    ;;do (let ([a (car t0)]) (let ([t0 (cdr t0)]) (let ([b (car t0)]) (let ([t0 (cdr t0)]) ......til end. 
    [`(lambda (,x0 ,xs ... . ,xrest) ,ebody)
     (define t0 (gensym 't))
     `(lambda ,t0
        ,(desugar-aux
          `(let* ,(foldr
                   (lambda (x acc) (append `([,x (car ,t0)] [,t0 (cdr ,t0)]) acc))
                   ;;init as `([,last ,t0])
                   `([,xrest ,t0])
                   (cons x0 xs))
             ,ebody)))]
    [`(if ,ec ,et ,ef) `(if ,(match ec
                               [`(promise? ,e) `(promise? ,e)]
                               [_ (desugar-aux ec)]) ,(desugar-aux et) ,(desugar-aux ef))]
    [`(and ,es ...)
     (match es
       [`() (desugar-aux `(quote #t))]
       [`(,e0) (desugar-aux e0)]
       [`(,e0 ,es ...) (desugar-aux `(if ,e0 (and ,@es) (quote #f)))])]
    ;;[_ (foldr (lambda (x acc) `(if ,x ,acc ,(desugar-aux `(quote #f)))) (desugar-aux `(quote #t)) (desugar-aux es))])]
    [`(or ,es ...)
     (match es
       [`() (desugar-aux `(quote #f))]
       [`(,e0) (desugar-aux e0)]
       [`(,e0 ,_) (desugar-aux e0)]
       [`(,e0 ,es ...) (desugar-aux `(if ,e0 (quote #t) (or ,@es)))])]
    ;;[_ (foldr (lambda (x acc) `(if ,x ,(desugar-aux `(quote #t)) ,acc)) (desugar-aux `(quote #f)) (desugar-aux es))])]
    [`(when ,ec ,et) (desugar-aux `(if ,ec ,et (prim void)))]
    [`(unless ,ec ,ef) (desugar-aux `(if ,ec (prim void) ,ef))]
    [`(begin ,es ...)
     (match es
       [`() (desugar-aux `(prim void))]
       [`(,e0) (desugar-aux e0)]
       ;;[`(,_ ,e0) (desugar-aux e0)]
       [`(,e0 ,es ...) (desugar-aux `(let ([,(gensym 't) ,e0]) (begin ,@es)))])]
    #|(define (remove-last l) (reverse (cdr (reverse l))))
     (desugar-aux `(let* ,(foldr (lambda (x acc)
                                   (append `([,(gensym (if (or (symbol? (last es))
                                                               (string? (last es)))
                                                           (last es)
                                                           (first (last es))))
                                              ,(desugar-aux x)]) acc)) '() (remove-last es)) ,(desugar-aux (last es))))]|#
    [`(cond ,clauses ...)
     (define args-d (map (lambda (e) (desugar-aux e)) clauses))
     (match args-d
       ['() `(prim void)]
       [_ (foldr (lambda (x acc)
                   (match x
                     [`(else ,expr) expr]
                     [`(,guard ,expr) `(if ,guard ,expr ,acc)]
                     [`(,guard) `(if ,guard ,guard (prim void))]))
                 '() args-d)])]
    [`(case ,key ,cases ...)
     (foldr (lambda (x acc)
              (match x
                [`(else ,expr)
                 (match expr
                   [`((lambda (,x) ,ebody) ,arg) (desugar-aux `(case ,arg ,@(cddr ebody)))]
                   [`(case ,key ,cases ...) (desugar-aux expr)]
                   [_ expr])]
                [`(,guard ,expr) `(if (quote ,(list? (member (eval-core (desugar-aux key)) guard))) ,expr ,acc)]))
            '() cases)]
    [`(set! ,x ,e) `(set! ,(desugar-aux x) ,(desugar-aux e))]
    [`(delay ,e) (desugar-aux `(prim vector (quote promise) (quote #f) (lambda () ,e)))]
    [`(force ,e)
     (define thunk (gensym 'thunk))
     (define delayed-val (gensym 'val))
     (desugar-aux `(let ([,thunk ,e])
                     (if (promise? ,thunk)
                         ;;check wheather or not the promise has been forced
                         (if (prim vector-ref ,thunk (quote 1))
                             ;;if forced, just return the value
                             (prim vector-ref ,thunk (quote 2))
                             ;;else set the bool to be #t
                             (begin (prim vector-set! ,thunk (quote 1) (quote #t))
                                    ;;just bind computed value with delayed-val
                                    (let ([,delayed-val ((prim vector-ref ,thunk (quote 2)) 'get)])
                                      ;;set value in thunk to be computed val
                                      (prim vector-set! ,thunk (quote 2) ,delayed-val))
                                    ;;finally return the computed val
                                    (prim vector-ref ,thunk (quote 2))))
                         ;;no error handling now, so just put #f
                         (if (number? ,thunk)
                             ,thunk
                             (quote "not promise")))))]
#|`(if ,(mcar p)
          ,(mcdr p)
          ,(desugar-aux `(begin ,(set-mcar! p `#t)
                                ,(set-mcdr! p (eval-core (desugar-aux (mcdr p))))
                                ,(mcdr p))))]|#
;; For this project, you may simply leave call/cc alone, we will
;; be handling it in subsequent projects.
[`(call/cc ,e) `(call/cc ,(desugar-aux e))]
#|[`(apply ,ef ,ex)
			 (define ef-d (desugar-aux ef))
			 (define ex-d (first (rest ex)))
			 (foldl (lambda (x acc)
					  `(,acc ,x)) ef-d ex-d)]|#
[`(apply ,ef ,ex)
 (match ef
   [(? prim? ef) `(apply-prim ,ef ,(desugar-aux ex))]
   [_ `(apply ,(desugar-aux ef) ,(desugar-aux ex))])]
[`(,(? prim? op) ,es ...)
 (define args `(,@es))
 (match op
   [`((promise? ,es)) (pretty-print es) `(promise? ,es)]
   [_ `(prim ,op ,@(foldr (lambda (x acc)
                            (if (prim? x)
                                (cons `(lambda args (apply-prim ,x args)) acc)
                                (cons (desugar-aux x) acc))) '() args))])]
; have to quote all the inner datums, the syntax is '#(1 2 3) => (prim vector '1 '2 '3)|#
[`#(,(? datum? dats) ...) (desugar-aux `(prim vector ,@(map (lambda (dat) (list 'quote dat)) dats)))]
#|[`(,e0 ,e1)
     (define args `(,e0 ,e1))
     (pretty-print "two args here" args)
     `(,@(foldr (lambda (x acc)
                  (match x
                    [(? prim? x)
                     (define args (gensym 'args))
                     (cons `(lambda ,args (apply-prim ,x ,args)) acc)]
                    [_ (cons (desugar-aux x) acc)])) '() args))]|#
[`(,es ...)
 (map (λ (e) (desugar-aux e)) es)]
#|(define args `(,@es))
     ;;(define args (map (lambda (e) (desugar-aux e)) es))
     ;;(pretty-print args)
     `(,@(foldr (lambda (x acc)
                  (match x
                    [(? prim? x) #:when (and (not (equal? (desugar-aux x) 'prim))
                                             (not (equal? x 'void))
                                             (not (equal? x 'car))
                                             (not (equal? x '<))
                                             (not (equal? x '(cond)))) (cons `(lambda args (apply-prim ,(desugar-aux x) args)) acc)]
                    [`(promise? ,e) (cons `(promise? ,e) acc)]
                    [_ (cons (desugar-aux x) acc)])) '() args))]|#
[else (error `(unexpected-syntax: ,e))]))
(define/contract (desugar e)
  (-> scheme-exp? core-exp?)
  (define (wrap e)
    `(let*
         (
          ;; TODO: you may want to define functions here that may be
          ;; then used by code you generate in desugar-aux. For
          ;; example, my implementation defines (at least)
          ;; %raise-handler, promise?, and force.
          [promise? (lambda (thunk) (and (vector? thunk)
                                         (equal? (vector-length thunk) '3)
                                         (equal? (vector-ref thunk '0) 'promise)))]
          )
       ,e))
  (desugar-aux (wrap e)))

; I, First Last, pledge on my honor that I have not given or 
; received any unauthorized assistance on this project.
