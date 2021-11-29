#lang racket
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Cif.rkt")
(require "type-check-Lvec.rkt")
(require "type-check-Cvec.rkt")
(require "type-check-Cfun.rkt")
(require "type-check-Lany.rkt")
(provide type-check-Cany type-check-Cany-mixin type-check-Cany-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cany


(define (type-check-Cany-mixin super-class)
  (class super-class
    (super-new)
    (inherit type-check-exp check-type-equal? combine-types)
  
  (define/override ((type-check-tail env block-env blocks) t)
    (match t
      [(IfStmt cnd tail1 tail2)
       (define-values (c Tc) ((type-check-exp env) cnd))
       (check-type-equal? Tc 'Boolean t)
       (define T1 ((type-check-tail env block-env blocks) tail1))
       (define T2 ((type-check-tail env block-env blocks) tail2))
       (check-type-equal? T1 T2 t)
       (combine-types T1 T2)]
      [(Exit) '_]
      [else ((super type-check-tail env block-env blocks) t)]))
  ))

(define type-check-Cany-class (type-check-Cany-mixin
                               (type-check-Cfun-mixin
                                (type-check-Cvec-mixin
                                 (type-check-Cif-mixin
                                  (type-check-Cvar-mixin
                                   type-check-Lany-class))))))

(define (type-check-Cany p)
  (send (new type-check-Cany-class) type-check-program p))
