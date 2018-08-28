#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-R0.rkt")
(require "interp-R1.rkt")
(require "interp.rkt")
(require "compiler.rkt")
;; (debug-level 0)
(debug-level 1)
;; (debug-level 4)

;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (or whatever file provides your passes)
;; should be named "compiler.rkt"
(define r0-passes
  `( ("flipper" ,flipper ,interp-R0)
     ("partial evaluator" ,pe-arith ,interp-R0)
     ))
(define r1-passes
  `( ("uniquify" ,(uniquify '()) ,(interp-R1 '()))
     ("flatten" ,flatten ,interp-C)
     ("instruction selection" ,select-instructions ,interp-x86)
     ("assign homes" ,(assign-homes (void)) ,interp-x86)
     ("insert spill code" ,patch-instructions ,interp-x86)
     ("print x86" ,print-x86 #f)
     ))


(interp-tests "r0" #f r0-passes interp-R0 "r0" (tests-for "r0"))
(interp-tests "r1" #f r1-passes (interp-R1 '()) "r1" (tests-for "r1"))
(compiler-tests "r1" #f r1-passes "r1" (tests-for "r1"))
(newline)(display "tests passed!") (newline)
