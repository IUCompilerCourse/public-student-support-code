#lang racket
(require "utilities.rkt")
(require "interp_Lwhile.rkt")
(require "interp_Cvar.rkt")
(require "interp_Cif.rkt")
(provide interp_Cwhile interp_Cwhile-mixin)

(define (interp_Cwhile-mixin super-class)
  (class super-class
    (super-new)
    (inherit interp_exp); call-function

    #;(define/override (apply-closure fun-val arg-vals e)
      (let ([f (vector-ref fun-val 0)])
        (call-function f (cons fun-val arg-vals) e)))
    
    (define/override ((interp-stmt env) s)
      (match s
        [(Prim op es)
         ((interp_exp env) s)
         env]
        [(Assign (Var x) e)
         (dict-set env x (box ((interp_exp env) e)))]
        [else ((super interp-stmt env) s)]))

    ))

(define Cwhile-class (interp_Cwhile-mixin
                     (interp_Cif-mixin
                      (interp_Cvar-mixin
                       interp_Lwhile-class))))

(define (interp_Cwhile p)
  (send (new Cwhile-class) interp-program p))
