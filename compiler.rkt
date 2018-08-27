#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-R0.rkt")
(require "interp-R1.rkt")
(require "interp.rkt")
(require "utilities.rkt")

;; This exports r0-passes and r1-passes, defined below, to users of this file.
(provide r0-passes r1-passes)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; R0 examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following pass is just a silly pass that doesn't change anything important,
;; but is nevertheless an example of a pass. It flips the arguments of +. -Jeremy
(define (flipper e)
  (match e
    [(? fixnum?) e]
    [`(read) `(read)]
    [`(- ,e1) `(- ,(flipper e1))]
    [`(+ ,e1 ,e2) `(+ ,(flipper e2) ,(flipper e1))]
    [`(program ,e) `(program ,(flipper e))]
    ))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (cond [(fixnum? r) (fx- 0 r)]
	[else `(- ,r)]))

(define (pe-add r1 r2)
  (cond [(and (fixnum? r1) (fixnum? r2)) (fx+ r1 r2)]
	[else `(+ ,r1 ,r2)]))

(define (pe-arith e)
  (match e
    [(? fixnum?) e]
    [`(read) `(read)]
    [`(- ,e1) (pe-neg (pe-arith e1))]
    [`(+ ,e1 ,e2) (pe-add (pe-arith e1) (pe-arith e2))]
    [`(program ,e) `(program ,(pe-arith e))]
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uniquify : env -> R1 -> R1
(define ((uniquify env) e)
  (error "TODO: code goes here"))

;; flatten : R1 -> C0-expr x (C0-stmt list) x var list
(define (flatten e)
  (error "TODO: code goes here"))

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions e)
  (error "TODO: code goes here"))

;; assign-homes : homes -> pseudo-x86 -> pseudo-x86
(define ((assign-homes homes) e)
  (error "TODO: code goes here"))

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions e)
  (error "TODO: code goes here"))

;; print-x86 : x86 -> string
(define (print-x86 e)
  (error "TODO: code goes here"))



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
