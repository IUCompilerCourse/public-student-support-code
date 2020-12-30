#lang racket
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Rif.rkt")
(provide type-check-Cif type-check-Cif-mixin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cif

(define (type-check-Cif-mixin super-class)
  (class super-class
    (super-new)
    (inherit type-check-exp check-type-equal?)

    (define/override ((type-check-tail env block-env G) t)
      (match t
        [(Goto label)
         ;; Memoization because G is a DAG -Jeremy
         (cond [(dict-has-key? block-env label)
                (dict-ref block-env label)]
               [else
                (define t ((type-check-tail env block-env G)
                           (dict-ref G label)))
                (dict-set! block-env label t)
                t])]
        [(IfStmt cnd tail1 tail2)
         (define-values (c Tc) ((type-check-exp env) cnd))
         (check-type-equal? Tc 'Boolean t)
         (define T1 ((type-check-tail env block-env G) tail1))
         (define T2 ((type-check-tail env block-env G) tail2))
         (check-type-equal? T1 T2 t)
         T1]
        [else ((super type-check-tail env block-env G) t)]))
    ))

(define type-check-Cif-class (type-check-Cif-mixin
                             (type-check-Cvar-mixin
                              type-check-Rif-class)))

(define (type-check-Cif p)
  (send (new type-check-Cif-class) type-check-program p))
  
