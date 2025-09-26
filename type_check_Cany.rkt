#lang racket
(require "utilities.rkt")
(require "type_check_Clambda.rkt")
(require "type_check_Lany.rkt")
(provide type_check_Cany type_check_Cany-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_Cany

(define type_check_Cany-class
  (class (type_check_any-mixin type_check_Clambda-class)
    (super-new)
    (inherit type_check_exp check-type-equal? join-types exp-ready?)

    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(ValueOf e ty) (recur e)]
        [else (super free-vars-exp e)]))
    
    (define/override ((type_check_tail env block-env blocks) t)
      (debug 'type_check_tail "Cany" t)
      (match t
        [(IfStmt cnd tail1 tail2)
         (cond [(exp-ready? cnd env)
                (define-values (c Tc) ((type_check_exp env) cnd))
                (check-type-equal? Tc 'Boolean t)
                ])
         (define T1 ((type_check_tail env block-env blocks) tail1))
         (define T2 ((type_check_tail env block-env blocks) tail2))
         (check-type-equal? T1 T2 t)
         (join-types T1 T2)]
        [else ((super type_check_tail env block-env blocks) t)]))
    ))

(define (type_check_Cany p)
  (send (new type_check_Cany-class) type_check_program p))
