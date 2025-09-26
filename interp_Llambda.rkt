#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp_Lfun.rkt")
(provide interp_Llambda interp_Llambda-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp_Llambda-class
  (class interp_Lfun-class
    (super-new)

    (define/override (interp-op op)
      (verbose "Llambda/interp-op" op)
      (match op
        ['procedure-arity
         (match-lambda
           [(Function xs body lam-env)  (length xs)]
           [v (error 'interp-op "Llambda/expected a function, not ~a" v)])]
        [else (super interp-op op)]))

    (define/override ((interp_exp env) e)
      (define recur (interp_exp env))
      (verbose "Llambda/interp_exp" e)
      (match e
        [(Lambda (list `[,xs : ,Ts] ...) rT body)
         (Function xs body env)]
        [(UncheckedCast e t)
         (recur e)]
        [else ((super interp_exp env) e)]))
    ))

(define (interp_Llambda p)
  (send (new interp_Llambda-class) interp-program p))
 
