#lang racket
(require "utilities.rkt")
(require "type-check-Cwhile.rkt")
(require "type-check-Lvec.rkt")
(provide type-check-Cvec type-check-Cvec-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cvec

(define type-check-Cvec-class
  (class (type-check-vec-mixin type-check-Cwhile-class)
    (super-new)
    (inherit check-type-equal? exp-ready? type-check-exp)

    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(HasType e t) (recur e)]
        [(Allocate size ty) (set)]
        [(GlobalValue name) (set)]
        [else (super free-vars-exp e)]))
        
    (define/override ((type-check-stmt env) s)
      (match s
        [(Collect size) (void)]
        [(Prim 'vector-set! (list vec index rhs))
         #:when (and (exp-ready? vec env) (exp-ready? index env)
                     (exp-ready? rhs env))
         ((type-check-exp env) s)]
        [else ((super type-check-stmt env) s)]))
    
    ))

(define (type-check-Cvec p)
  (send (new type-check-Cvec-class) type-check-program p))


  
