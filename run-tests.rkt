#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-Rvar.rkt")
(require "interp-Cvar.rkt")
(require "interp.rkt")
(require "compiler.rkt")
;; (debug-level 1)
;; (AST-output-syntax 'concrete-syntax)

<<<<<<< HEAD
;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; should be named "compiler.rkt"
(define passes
  (list (list "uniquify" uniquify interp-Rvar type-check-Rvar)
     ;; Uncomment the following passes as you finish them.
     ;; (list "remove complex opera*" remove-complex-opera* interp-Rvar type-check-Rvar)
     ;; (list "explicate control" explicate-control interp-Cvar type-check-Cvar)
     ;; (list "instruction selection" select-instructions interp-pseudo-x86-0 )
     ;; (list "assign homes" assign-homes interp-x86-0)
     ;; (list "patch instructions" patch-instructions interp-x86-0)
     ;; (list "print x86" print-x86 #f)
     ))

=======
>>>>>>> 7ca533ec522361719dfe89a6be6325302f22c92f
;; all the files in the tests/ directory with extension ".rkt".
(define all-tests
  (map (lambda (p) (car (string-split (path->string p) ".")))
       (filter (lambda (p)
                 (string=? (cadr (string-split (path->string p) ".")) "rkt"))
               (directory-list (build-path (current-directory) "tests")))))

(define (tests-for r)
  (map (lambda (p)
         (caddr (string-split p "_")))
       (filter
        (lambda (p)
          (string=? r (car (string-split p "_"))))
        all-tests)))

(interp-tests "var" #f compiler-passes interp-Rvar "var_test" (tests-for "var"))

;; Uncomment the following when all the passes are complete to
;; test the final x86 code.
;; (compiler-tests "var" #f compiler-passes "var_test" (tests-for "var"))

