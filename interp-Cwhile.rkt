#lang racket
(require "utilities.rkt")
(require "interp-Lwhile.rkt")
(require "interp-Cvar.rkt")
(require "interp-Cif.rkt")
(provide interp-Cwhile interp-Cwhile-mixin)

(define (interp-Cwhile-mixin super-class)
  (class super-class
    (super-new)
    (inherit interp-exp); call-function

    #;(define/override (apply-closure fun-val arg-vals e)
      (let ([f (vector-ref fun-val 0)])
        (call-function f (cons fun-val arg-vals) e)))
    
    (define/override ((interp-stmt env) s)
      (match s
        [(Prim op es)
         ((interp-exp env) s)
         env]
        [(Assign (Var x) e)
         (dict-set env x (box ((interp-exp env) e)))]
        [else ((super interp-stmt env) s)]))

    ))

(define Cwhile-class (interp-Cwhile-mixin
                     (interp-Cif-mixin
                      (interp-Cvar-mixin
                       interp-Lwhile-class))))

(define (interp-Cwhile p)
  (send (new Cwhile-class) interp-program p))
