#lang racket

(provide assignment-convert
         alphatize
         anf-convert
         cps-convert)

(require "utils.rkt")

; The output of assignment 2:
; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (e e ...)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (set! x e)
;     | (call/cc e)
;     | x
;     | (quote dat)

;; PASS 1 -- Assignment conversion and Alphatization

;; You may wish to use this helper function that computes if a
;; variable x is mutated in the expression e. A simpler solution would
;; be to box _all_ variables, so no check is needed. However, this
;; will come with significant runtime performance cost.
(define (mutated? x e)
  (match e
    [`(let ([,xs ,es] ...) ,ebody)
     ; if its mutated in es is has been mutated
     ; elif its shadowed by xs its not mutated.
     ; elif its mutated in ebody
     (or (ormap (λ (e) (mutated? x e)) es)
         (and (not (member x xs)) (mutated? x ebody)))]
    [`(lambda (,xs ...) ,ebody)
     ; if its shadowed by xs its not mutated
     ; elif its mutated in ebody
     (and (not (member x xs)) (mutated? x ebody))]
    [`(lambda ,xarg ,ebody)
     ; same as the multi-arg lambda case.
     (and (not (equal? x xarg)) (mutated? x ebody))]
    [`(apply ,ef ,ex) (or (mutated? x ef) (mutated? x ex))]
    [`(prim ,op ,es ...) (ormap (λ (e) (mutated? x e)) es)]
    [`(apply-prim ,op ,ex) (mutated? x ex)]
    [`(call/cc ,e) (mutated? x e)]
    [`(set! ,q ,e)
     (or (equal? x q) (mutated? x e))]
    [`(if ,ec ,et ,ef) (or (mutated? x ec) (mutated? x et) (mutated? x ef))]
    [`(,es ...) (ormap (λ (e) (mutated? x e)) es)]
    [else #f]))
;;traversing the expression to find potential mutables, and return as a set. 
(define (mutvars-gen e)
  (define (h e)
    (match e
      [`(let ([,xs ,es] ...) ,ebody)
       (define g-bindings (foldl (lambda (e acc) (append (h e) acc)) '() es))
       (define g-body (h ebody))
       (append g-bindings g-body)]
      [`(lambda ,x ,ebody) (h ebody)]
      [`(apply ,ef ,ex) (append (h ef) (h ex))]
      [`(prim ,(? prim? op) ,es ...) (foldl (lambda (e acc) (append (h e) acc)) '() es)]
      [`(apply-prim ,(? prim? op) ,ex) (h ex)]
      [`(if ,ec ,et ,ef) (append (h ec) (h et) (h ef))]
      [`(set! ,x ,e) (append (list x) (h e))]
      [`(call/cc ,e) (h e)]
      [(? symbol? x) (list)]
      [`(quote ,(? datum? dat)) (list)]
      [`(,es ...)
       (foldl (lambda (e acc) (append (h e) acc)) '() es)]
      [else (error `(unexpected-syntax: ,e))]))
  (list->set (h e)))

;;optimized mutated? for avoiding checking every variable in the expression
;;we just specifically checking variables with same varv name in
;;the set of potential mutables we generated  
(define (f-mutated? mutvars x e)
  (if (set-member? mutvars x)
      (mutated? x e)
      #f))

;; Convert assignments (use of the set! operation) into calls using
;; the primitive vector operations.
;;
;; Box mutated vars at initialization,
;; e.g., (let ([x '()]) ...) -> (let ([x (prim make-vector '1 '())]) ...)
;;
;; What happens to (lambda (x) ...) if x is mutated?
;; .. all all other forms in the language ...
;; (set! x '12) should become something akin to (prim vector-set! x '0 '12)
(define/contract (assignment-convert e)
  (-> core-exp? assignment-converted-exp?)
  ;; convert instances of set! to vector-set!
  ;; and ensure references to boxed variables are properly vector-ref'd.
  ;;generate a set of potential mutables in the whole expression
  (define mutvars (mutvars-gen e))
  (define (make-box e muts)
    (match e
      [`(let ([,xs ,es] ...) ,ebody)
       ;;To update a list of newly appeared mutables by traversing down to its body to filter for mutables in its ids in the bindings
       (define upmuts (list->set (filter (lambda (x) (f-mutated? mutvars x ebody)) xs)))
       ;;simply substract all the updated mutables from its ids in the bindings to get all the immuts updated
       (define upimmuts (set-subtract (list->set xs) upmuts))
       ;;to find all the potential mutables in the body of expr is to aggregate all the previous mutables
       ;;and updated mutables, then make all of them unique, then substract all the updated immuts
       ;;from the aggregated set. Cuz you never know some of the expressions above the current let would
       ;;contain mutables which may need to be boxed in the body of current let.(updated mutables)
       (define bmuts (set-subtract (set-union muts upmuts) upimmuts))
       (define (alloc-new-muts x e upmuts muts)
         (if (set-member? upmuts x)
             `(prim make-vector '1 ,(make-box e muts))
             (make-box e muts)))
       `(let ,(foldr (lambda (x e acc) (cons `(,x ,(alloc-new-muts x e upmuts muts)) acc)) '() xs es)
          ;;keep going down to match the ebody with
          ;;all the potential muts in the set of bmuts
          ,(make-box ebody bmuts))]
      [`(lambda (,xs ...) ,ebody)
       ;;do a regular check for upmuts, upimmuts and bmuts
       (define upmuts (list->set (filter (lambda (x) (f-mutated? mutvars x ebody)) xs)))
       (define upimmuts (set-subtract (list->set xs) upmuts))
       (define bmuts (set-subtract (set-union muts upmuts) upimmuts))
       ;;if upmuts is an empty set
       (if (set-empty? upmuts)
           ;;we simply go down to its body to keep seeking mutables to box
           ;;becuse nothing in the lambda bindings is bound (all immuts)
           `(lambda ,xs ,(make-box ebody bmuts))
           ;;if yes, use let to make box of all the mutables, push them on the heap
           ;;by making them object, and initailize the object, then go down to its body.
           `(lambda ,xs (let ,(foldr (lambda (m acc) (cons `[,m (prim make-vector '1 ,m)] acc))
                                     '() (set->list upmuts)) ,(make-box ebody bmuts))))]
      [`(lambda ,x ,ebody)
       (define if-x-mutated `(lambda ,x (let ,(foldr (lambda (m acc)
                                                       (cons `[,m (prim make-vector '1 ,m)] acc))
                                                     '() `(,x))
                                          ,(make-box ebody (set-add muts x)))))
       (define if-x-immutated `(lambda ,x ,(make-box ebody (set-remove muts x))))
       (if (f-mutated? mutvars x ebody)
           if-x-mutated
           if-x-immutated)]
      [`(apply ,ef ,ex) `(apply ,(make-box ef muts) ,(make-box ex muts))]
      [`(prim ,(? prim? op) ,es ...) `(prim ,op ,@(map (lambda (e) (make-box e muts)) es))]
      [`(apply-prim ,(? prim? op) ,ex) `(apply-prim ,op ,(make-box ex muts))]
      [`(if ,ec ,et ,ef) `(if ,(make-box ec muts) ,(make-box et muts) ,(make-box ef muts))]
      [`(set! ,x ,e) `(prim vector-set! ,x '0 ,(make-box e muts))]
      [`(call/cc ,e) `(call/cc ,(make-box e muts))]
      [`(quote ,(? datum? dat)) `(quote ,dat)]
      [(? symbol? x)
       ;;do a check here to see if symbol x is existing in the old set of mutables
       (if (set-member? muts x)
           ;;if so, we retrive its value
           `(prim vector-ref ,x '0)
           ;;otherwise we simply return itself
           x)]
      [`(,e0 ,es ...) (map (lambda (e) (make-box e muts)) (cons e0 es))]
      [else (error `(unexpected-syntax: ,e))]))
  ;;start with empty set since there is no mutables found at the beginning
  (make-box e (set)))

; assignment-convert =>

;;; set! is removed and replaced with vector-set!
; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (e e ...)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (call/cc e)
;     | x
;     | (quote dat)

;; alphatize both takes and produces this language as well

;; an alphatized program should contain no variable shadowing.
;; The following is an illegal program after alphatization
;; (lambda (x) (lambda (x) x))
;; as x is shadowed in the inner lambda. One of the variables must
;; be renamed to become legal.
(define/contract (alphatize e)
  (-> assignment-converted-exp? alphatized-exp?)
  (define (h e env)
    (match e
      [`(let ([,xs ,es] ...) ,ebody)
       (define new-bnd (foldr (lambda (x e acc) (cons `[,(gensym x) ,(h e env)] acc)) '() xs es))
       (define new-env (foldl (lambda (x nb env) (hash-set env x (car nb))) env xs new-bnd))
       `(let ,new-bnd ,(h ebody new-env))]
      [`(lambda (,xs ...) ,ebody)
       (define new-ids (foldr (lambda (x acc) (append `(,(gensym x)) acc)) '() xs))
       (define new-env (foldl (lambda (x new-id env) (hash-set env x new-id)) env xs new-ids))
       `(lambda ,new-ids ,(h ebody new-env))]
      [`(lambda ,x ,ebody)
       (define new-x (gensym x))
       (define new-env (hash-set env x new-x))
       `(lambda ,new-x ,(h ebody new-env))]
      [`(apply ,ef ,ex) `(apply ,(h ef env) ,(h ex env))]
      [`(prim ,(? prim? op) ,es ...) `(prim ,op ,@(map (lambda (e) (h e env)) es))]
      [`(apply-prim ,(? prim? op) ,ex) `(apply-prim ,op ,(h ex env))]
      [`(if ,ec ,et ,ef) `(if ,(h ec env) ,(h et env) ,(h ef env))]
      [`(call/cc ,e) `(call/cc ,(h e env))]
      [`(quote ,(? datum? dat)) `(quote ,dat)]
      [(? symbol? x) (hash-ref env x)]
      [`(,e0 ,es ...) (map (lambda (x) (h x env)) (cons e0 es))]
      [else (error `(unexpected-syntax: ,e))]))
  (h e (hash)))


;; PASS 2 -- ANF conversion
(define (atomic? exp)
  (match exp
    [`(quote ,_)         #t]
    [(? number?)         #t]
    [(? boolean?)        #t]
    [(? string?)         #t]
    [(? char?)           #t]
    [(? symbol?)         #t]
    [(or '+ '- '* '/ '=) #t]
    [_                   #f]))
;; Converts to ANF; adapted from Flanagan et al, 1993 "The Essence of Compiling With Continuations"
(define/contract (anf-convert e)
  (-> alphatized-exp? anf-exp?)
  
  (define (normalize-term exp) (normalize exp (lambda (x) x)))
  
  (define (normalize-name exp k)
    (normalize exp (λ (aexp) 
                     (if (atomic? aexp)
                         (k aexp) 
                         (let ([t (gensym 'anf-bind)]) 
                           `(let ([,t ,aexp]) ,(k t)))))))

  (define (normalize-name* exp* k)
    (if (null? exp*)
        (k '())
        (normalize-name (car exp*) (lambda (t) 
                                     (normalize-name* (cdr exp*) (lambda (t*) 
                                                                   (k `(,t . ,t*))))))))

  
  ;; normalize
  (define (normalize e k)
    (match e
      [(? atomic?) (k e)]
      [`(lambda ,params ,body)   
       (k `(lambda ,params ,(normalize-term body)))]
      [`(let () ,exp)
       (normalize exp k)]
      [`(let ([,x ,exp1] . ,clause) ,exp2) 
       (normalize exp1 (λ (aexp1) 
                         `(let ([,x ,aexp1])
                            ,(normalize `(let (,@clause) ,exp2) k))))]
      [`(if ,exp1 ,exp2 ,exp3)    
       (normalize-name exp1 (λ (t) 
                              (k `(if ,t ,(normalize-term exp2) 
                                      ,(normalize-term exp3)))))]
      [`(call/cc ,exp) (normalize-name exp (lambda (aexp) (k `(call/cc ,aexp))))]
      [`(apply-prim ,op ,args) (normalize-name args (lambda (aexp) (k `(apply-prim ,op ,aexp))))]
      [`(apply ,ef ,ex) (normalize-name* `(,ef ,ex) (lambda (aes) (k `(apply ,@aes))))]
      [`(prim ,(? prim? op) ,es ...) (normalize-name* es (lambda (aes) (k `(prim ,op ,@aes))))]
      [`(,f . ,e*)
       (normalize-name f (λ (t) 
                           (normalize-name* e* (λ (t*)
                                                 (k `(,t . ,t*))))))]
      [else (error `(unexpected-syntax: ,e))]))
  (normalize-term e))

; anf-convert =>
; e ::= (let ([x e]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (prim op ae ...)
;     | (apply-prim op ae)
;     | (if ae e e)
;     | (call/cc ae)
;     | ae
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)

;; PASS 3 -- CPS conversion

;; CPS Conversion: eliminates call/cc by transformation to
;; continuation-passing style. We will code significant parts of the
;; solution to this pass in class.
(define/contract (cps-convert e)
  (-> anf-exp? cps-exp?)
  (define (T-ae e)
    (match e
      [(? symbol? x) x]
      [`',dat `',dat]
      [`(lambda (,xs ...) ,ebody)
       (define kvar (gensym 'k))
       `(lambda (,kvar ,@xs) ,(T ebody kvar))]
      [`(lambda ,x ,ebody)
       (define argvar (gensym x))
       (define kvar (gensym 'k))
       `(lambda ,argvar (let ([,kvar (prim car ,argvar)])
                          (let ([,x (prim cdr ,argvar)])
                            ,(T ebody kvar))))]))
  (define (T e cae)
    (match e
      ; When we hit an atomic expression, we would usually return
      ; but in CPS, all we can do is call the continuation.
      [(or `(lambda ,_ ,_) (? symbol?) `',_) `(,cae '0 ,(T-ae e))]
      ;;; These patterns are moderately unnecessary, but save some parsing later
      ; We can catch let bindings that will be legal in CPS and not over-transform them.
      [`(let ([,x (prim ,op ,aes ...)]) ,ebody)
       `(let ([,x (prim ,op ,@(map T-ae aes))]) ,(T ebody cae))]
      [`(let ([,x (apply-prim ,op ,aex)]) ,ebody)
       `(let ([,x (apply-prim ,op ,(T-ae aex))]) ,(T ebody cae))]
      [`(let ([,x `(lambda ,xa ,ef)]) ,ebody)
       `(let ([,x (lambda ,xa ,ef)]) ,(T ebody cae))]
      [`(let ([,x ',dat]) ,ebody)
       `(let ([,x ',dat]) ,(T ebody cae))]
      ;;; end 'unnecessary' let patterns.
      ; bind all possible RHS of lets to a continuation-extending call
      [`(let ([,x ,e]) ,ebody)
       (T e `(lambda (,(gensym '_k) ,x) ,(T ebody cae)))]
      ; prim must be lifted into a let binding to become valid CPS. This
      ; will help during the final transformation into the target language.
      [`(prim ,op ,aes ...)
       (define bndvar (gensym 'cpsprim))
       `(let ([,bndvar (prim ,op ,@(map T-ae aes))]) ,(T bndvar cae))]
      ; same is true for apply-prim
      [`(apply-prim ,op ,aex)
       (define bndvar (gensym 'cpsaprim))
       `(let ([,bndvar (apply-prim ,op ,(T-ae aex))]) ,(T bndvar cae))]
      ; here we truly define call-with-current-continuation.
      ; we are finally *calling* the function with the current continuation
      ; as the argument! Of course, if the continuation is never explicitly
      ; a continuation is needed to 'return' with. So use the current continuation there as well.
      [`(call/cc ,aef)
       `(,(T-ae aef) ,cae ,cae)]
      ; If is a simple enough transformation, we just call the correct T
      ; function as needed and move along.
      [`(if ,ec ,aet ,aef)
       `(if ,(T-ae ec) ,(T aet cae) ,(T aef cae))]
      ; apply is allowed in the output language.
      ; but we must add the continuation to the given list.
      [`(apply ,aef ,aex)
       (define argvar (gensym 'cpsargs))
       `(let ([,argvar (prim cons ,cae ,(T-ae aex))])
          (apply ,(T-ae aef) ,argvar))]
      ; When calling a function, we need to give the continuation now
      [`(,aef ,aes ...) `(,(T-ae aef) ,cae ,@(map T-ae aes))]))
  (T e '(lambda (k x) (let ([_die (prim halt x)])
                        (k x)))))
  


; cps-convert =>

; e ::= (let ([x (apply-prim op ae)]) e)
;     | (let ([x (prim op ae ...)]) e)
;     | (let ([x (lambda x e)]) e)
;     | (let ([x (lambda (x ...) e)]) e)
;     | (let ([x (quote dat)]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (if ae e e)
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)
; cps-convert =>

; e ::= (let ([x (apply-prim op ae)]) e)
;     | (let ([x (prim op ae ...)]) e)
;     | (let ([x (lambda x e)]) e)
;     | (let ([x (lambda (x ...) e)]) e)
;     | (let ([x (quote dat)]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (if ae e e)
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)
  
(define cps '(lambda (thunk) (and (vector? thunk)
                                  (equal? (vector-length thunk) '3)
                                  (equal? (vector-ref thunk '0) 'promise))))
(define test-2 '(+
                 (foldl +
                        '0
                        (map (lambda (b) (if b '1 '2)) 
                             (map promise?
                                  (list
                                   '#f
                                   '#t
                                   (delay '0)
                                   (list)
                                   (list '() '())
                                   '#()
                                   '#(0 1)
                                   'yes))))
 
                 (let ([x '0])
                   (let ([p (delay (begin (set! x (+ '1 x)) x))])
                     (let ([v (+ (force p) (force p) (force p))])
                       (+ v x))))))

(define test-4 '(map promise?
                     (list
                      ;;'#f
                      ;;'#t
                      ;;(delay '0)
                      ;;(list)
                      ;;(list '() '())
                      ;;'#()
                      ;;'#(0 1)
                      ;;'yes)))
                      )))
(define  c `(promise? '()))
 
#|(let ([x '0])
                   (let ([p (delay (begin (set! x (+ '1 x)) x))])
                     (let ([v (+ (force p) (force p) (force p))])
                       (+ v x))))))|#

(define test-3 '(+
                 (apply (lambda (x y) (- y x)) '(4 10))
                 ((apply (lambda (u)
                           (lambda (v w)
                             (* u (+ v w))))
                         '(4))
                  '5 '6)))
;;(eval-core (anf-convert (assignment-convert (simplify-core (desugar test-2)))))
;;(cps-convert (anf-convert (assignment-convert (desugar c))))
;;(eval-core (cps-convert (anf-convert (assignment-convert (simplify-core (desugar c))))))
;;(cps-convert (anf-convert (assignment-convert (simplify-core (desugar test-2)))))

; cps-convert =>

; e ::= (let ([x (apply-prim op ae)]) e)
;     | (let ([x (prim op ae ...)]) e)
;     | (let ([x (lambda x e)]) e)
;     | (let ([x (lambda (x ...) e)]) e)
;     | (let ([x (quote dat)]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (if ae e e)
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)