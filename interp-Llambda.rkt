#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp-Lfun.rkt")
(provide interp-Llambda interp-Llambda-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp-Llambda-class
  (class interp-Lfun-class
    (super-new)

    (define/override (interp-op op)
      (verbose "Llambda/interp-op" op)
      (match op
        ['procedure-arity
         (match-lambda
           [(Function xs body lam-env)  (length xs)]
           [v (error 'interp-op "Llambda/expected a function, not ~a" v)])]
        [else (super interp-op op)]))

    (define/override ((interp-exp env) e)
      (define recur (interp-exp env))
      (verbose "Llambda/interp-exp" e)
      (match e
        [(Lambda (list `[,xs : ,Ts] ...) rT body)
         (Function xs body env)]
        [(UncheckedCast e t)
         (recur e)]
        [else ((super interp-exp env) e)]))
    ))

(define (interp-Llambda p)
  (send (new interp-Llambda-class) interp-program p))
 
