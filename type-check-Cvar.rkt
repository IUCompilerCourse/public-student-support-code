#lang racket
(require "utilities.rkt" "type-check-Rvar.rkt")
(provide type-check-Cvar type-check-Cvar-mixin)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cvar

(define (type-check-Cvar-mixin super-class)
  (class super-class
    (super-new)
    (inherit type-check-exp type-equal? check-type-equal?)

    (define/public ((type-check-stmt env) s)
      (match s
        [(Assign (Var x) e)
         (define-values (e^ t) ((type-check-exp env) e))
         (cond [(dict-has-key? env x)
                (check-type-equal? t (dict-ref env x) s)]
               [else (dict-set! env x t)])]))

    (define/public ((type-check-tail env block-env G) t)
      (match t
        [(Return e)
         (define-values (e^ t) ((type-check-exp env) e))
         t]
        [(Seq s t)
         ((type-check-stmt env) s)
         ((type-check-tail env block-env G) t)]))

    (define/override (type-check-program p)
      (match p
        [(CProgram info G)
         (define env (make-hash))
         (define block-env (make-hash))
         (define t ((type-check-tail env block-env G)
                    (dict-ref G 'start)))
         (unless (type-equal? t 'Integer)
           (error "return type of program must be Integer, not" t))
         (define locals-types (for/list ([(x t) (in-dict env)])
                                (cons x t)))
         (define new-info (dict-set info 'locals-types locals-types))
         (CProgram new-info G)]
        [else (super type-check-program p)]))
    ))

(define type-check-Cvar-class (type-check-Cvar-mixin type-check-Rvar-class))

(define (type-check-Cvar p)
  (send (new type-check-Cvar-class) type-check-program p))
