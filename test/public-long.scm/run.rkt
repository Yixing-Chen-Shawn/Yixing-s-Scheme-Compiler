#lang racket

(require (only-in "../../desugar.rkt" desugar))
(require (only-in "../../cps.rkt" assignment-convert alphatize anf-convert cps-convert))
(require (only-in "../../closure-convert.rkt" closure-convert proc->llvm))
(require (only-in "../../utils.rkt" run-llvm eval-scheme eval-core eval-proc eval-llvm simplify-core))

(match-define (list test-name eval-input transform eval-output)
  ; If we are getting .scm code, we test all phases of this project.
  (cond [(file-exists? "testcase.scm") (list "testcase.scm" eval-scheme
                                             (λ (scm)
                                               (proc->llvm
                                                (closure-convert
                                                 (cps-convert
                                                  (anf-convert
                                                   (alphatize
                                                    (assignment-convert
                                                     (simplify-core (desugar scm)))))))))
                                             (λ (x) (run-llvm x "../..")))]
        ; If we are getting .cps code, we test closure conversion.
        [(file-exists? "testcase.cps") (list "testcase.cps" eval-core closure-convert eval-proc)]))

; put the expected answer into `answer` and the given output to `output`.
(define prog (with-input-from-file test-name read))
(with-output-to-file "output" (λ () (print (eval-output (transform prog)))) #:exists 'replace)
