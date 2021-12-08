#lang racket
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Cif.rkt")
(require "type-check-Cwhile.rkt")
(require "type-check-Lvec.rkt")
(require "type-check-Cvec.rkt")
(require "type-check-Cfun.rkt")
(require "type-check-Clambda.rkt")
(require "type-check-Lany.rkt")
(provide type-check-Cany type-check-Cany-mixin type-check-Cany-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cany


(define (type-check-Cany-mixin super-class)
  (class super-class
    (super-new)
    (inherit check-type-equal? join-types exp-ready?)

    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(ValueOf e ty) (recur e)]
        [(Exit) (set)]
        [else (super free-vars-exp e)]))
    
    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "Cany" e)
        (define recur (type-check-exp env))
        (match e
          [(ValueOf e ty)
           (define-values (new-e e-ty) (recur e))
           (values (ValueOf new-e ty) ty)]
          [(Prim 'any-vector-length (list e1))
           (define-values (e1^ t1) (recur e1))
           (check-type-equal? t1 'Any e)
           (values (Prim 'any-vector-length (list e1^)) 'Integer)]
          [(Prim 'any-vector-ref (list e1 e2))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (check-type-equal? t1 'Any e)
           (check-type-equal? t2 'Integer e)
           (values (Prim 'any-vector-ref (list e1^ e2^)) 'Any)]
          [(Prim 'any-vector-set! (list e1 e2 e3))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (define-values (e3^ t3) (recur e3))
           (check-type-equal? t1 'Any e)
           (check-type-equal? t2 'Integer e)
           (check-type-equal? t3 'Any e)
           (values (Prim 'any-vector-set! (list e1^ e2^ e3^)) 'Void)]
          [else ((super type-check-exp env) e)])))
    
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
        [(Exit) '_]
        [else ((super type-check-tail env block-env blocks) t)]))
    ))

(define type-check-Cany-class (type-check-Cany-mixin
                               (type-check-Clambda-mixin
                                (type-check-Cfun-mixin
                                 (type-check-Cvec-mixin
                                  (type-check-Cwhile-mixin
                                   (type-check-Cif-mixin
                                    (type-check-Cvar-mixin
                                     type-check-Lany-class
                                     ))))))))

(define (type-check-Cany p)
  (send (new type-check-Cany-class) type-check-program p))
