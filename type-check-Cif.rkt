#lang racket
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Lif.rkt")
(provide type-check-Cif type-check-Cif-class type-check-Cif-mixin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cif

(define (type-check-Cif-mixin super-class)
  (class super-class
    (super-new)
    (inherit check-type-equal?)

    (define/override ((type-check-atm env) e)
      (match e
        [(Bool b) (values (Bool b) 'Boolean)]
        [else
         ((super type-check-atm env) e)]
        ))
    
    (define/override ((type-check-exp env) e)
      (debug 'type-check-exp "Cif ~a" e)
      (match e
        [(Bool b) (values (Bool b) 'Boolean)]
        [(Prim 'eq? (list e1 e2))
         (define-values (e1^ T1) ((type-check-exp env) e1))
         (define-values (e2^ T2) ((type-check-exp env) e2))
         (check-type-equal? T1 T2 e)
         (values (Prim 'eq? (list e1^ e2^)) 'Boolean)]
        [else
         ((super type-check-exp env) e)]
        ))
    
    (define/override ((type-check-tail env block-env blocks) t)
      (debug 'type-check-tail "Cif" t)
      (match t
        [(Goto label)
         ;; Memoization because blocks is a DAG -Jeremy
         (cond [(dict-has-key? block-env label)
                (dict-ref block-env label)]
               [else
                (define t ((type-check-tail env block-env blocks)
                           (dict-ref blocks label)))
                (dict-set! block-env label t)
                t])]
        [(IfStmt cnd tail1 tail2)
         (define-values (c Tc) ((type-check-exp env) cnd))
         (check-type-equal? Tc 'Boolean t)
         (define T1 ((type-check-tail env block-env blocks) tail1))
         (define T2 ((type-check-tail env block-env blocks) tail2))
         (check-type-equal? T1 T2 t)
         T1]
        [else ((super type-check-tail env block-env blocks) t)]))
    ))

(define type-check-Cif-class (type-check-Cif-mixin
                              (type-check-Cvar-mixin
                               type-check-Lif-class)))

(define (type-check-Cif p)
  (send (new type-check-Cif-class) type-check-program p))
  
