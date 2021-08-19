#lang racket
(require "utilities.rkt")
(require "interp-Rwhile-proxy-closure.rkt")
(require "interp-Cvar.rkt")
(require "interp-Cif.rkt")
(require "interp-Cvec.rkt")
(require "interp-Cfun.rkt")
(require "interp-Clambda.rkt")
(provide interp-Cwhile interp-Cwhile-mixin)

(define (interp-Cwhile-mixin super-class)
  (class super-class
    (super-new)
    (inherit interp-exp call-function)

    (define/override (apply-closure fun-val arg-vals e)
      (let ([f (vector-ref fun-val 0)])
        (call-function f (cons fun-val arg-vals) e)))
    
    (define/override ((interp-stmt env) s)
      (match s
        [(Prim 'read '())
         ((interp-exp env) s)
         env]
        [(Prim 'vector-set! (list e-vec i e-arg))
         ((interp-exp env) s)
         env]
        [(Prim 'any-vector-set! (list e-vec i e-arg))
         ((interp-exp env) s)
         env]
        [(Call e es)
         (define f-val ((interp-exp env) e))
         (define arg-vals (map (interp-exp env) es))
         (call-function f-val arg-vals s)
         env]
        [else ((super interp-stmt env) s)]))

    ))

(define Cwhile-class (interp-Cwhile-mixin
                  (interp-Clambda-mixin
                   (interp-Cfun-mixin
                    (interp-Cvec-mixin
                     (interp-Cif-mixin
                      (interp-Cvar-mixin
                       interp-Rwhile-proxy-closure-class)))))))

(define (interp-Cwhile p)
  (send (new Cwhile-class) interp-program p))
