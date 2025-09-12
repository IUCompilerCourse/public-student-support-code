#lang racket
(require "utilities.rkt")
(require "type_check_Cwhile.rkt")
(require "type_check_Lvec.rkt")
(provide type_check_Cvec type_check_Cvec-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_Cvec

(define type_check_Cvec-class
  (class (type_check_vec-mixin type_check_Cwhile-class)
    (super-new)
    (inherit check-type-equal? exp-ready? type_check_exp)

    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(HasType e t) (recur e)]
        [(Allocate size ty) (set)]
        [(GlobalValue name) (set)]
        [else (super free-vars-exp e)]))
        
    (define/override ((type_check_stmt env) s)
      (match s
        [(Collect size) (void)]
        [(Prim 'vector-set! (list vec index rhs))
         #:when (and (exp-ready? vec env) (exp-ready? index env)
                     (exp-ready? rhs env))
         ((type_check_exp env) s)]
        [else ((super type_check_stmt env) s)]))
    
    ))

(define (type_check_Cvec p)
  (send (new type_check_Cvec-class) type_check_program p))


  
