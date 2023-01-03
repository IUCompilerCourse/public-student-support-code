#lang racket
(require "utilities.rkt")
(require "type-check-Clambda.rkt")
(require "type-check-Lany.rkt")
(provide type-check-Cany type-check-Cany-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cany

(define type-check-Cany-class
  (class (type-check-any-mixin type-check-Clambda-class)
    (super-new)
    (inherit type-check-exp check-type-equal? join-types exp-ready?)

    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(ValueOf e ty) (recur e)]
        [else (super free-vars-exp e)]))
    
    (define/override ((type-check-tail env block-env blocks) t)
      (debug 'type-check-tail "Cany" t)
      (match t
        [(IfStmt cnd tail1 tail2)
         (cond [(exp-ready? cnd env)
                (define-values (c Tc) ((type-check-exp env) cnd))
                (check-type-equal? Tc 'Boolean t)
                ])
         (define T1 ((type-check-tail env block-env blocks) tail1))
         (define T2 ((type-check-tail env block-env blocks) tail2))
         (check-type-equal? T1 T2 t)
         (join-types T1 T2)]
        [else ((super type-check-tail env block-env blocks) t)]))
    ))

(define (type-check-Cany p)
  (send (new type-check-Cany-class) type-check-program p))
