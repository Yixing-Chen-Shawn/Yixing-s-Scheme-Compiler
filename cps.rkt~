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


;; convert assignments (use of the set! operation) into calls using
;; the primitive vector operations.
;;
;; box mutated vars at initialization,
;; e.g., (let ([x '()]) ...) -> (let ([x (prim make-vector '1 '())]) ...)
;; What happens to (lambda (x) ...) if x is mutated?
;; .. all all other forms in the language ...
;; (set! x '12) should become something akin to (prim vector-set! x '0 '12)
(define/contract (assignment-convert e)
  (-> core-exp? assignment-converted-exp?)
  'todo)

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
  ;; Defining curried rename function is convenient for mapping over lists of es
  'todo)


;; Converts to ANF; adapted from Flanagan et al, 1993 "The Essence of Compiling With Continuations"
(define/contract (anf-convert e)
  (-> alphatized-exp? anf-exp?)
  'todo)

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


(define/contract (cps-convert e)
  (-> anf-exp? cps-exp?)
  'todo)


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
