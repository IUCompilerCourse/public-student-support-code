#lang racket
(require "utilities.rkt" "type-check-Lvar.rkt")
(provide type-check-Cvar type-check-Cvar-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cvar

(define type-check-Cvar-class
  (class (type-check-var-mixin object%)
    (super-new)
    (inherit type-check-op type-equal? check-type-equal? type-check-exp)

    (define/public ((type-check-atm env) e)
      (match e
        [(Var x)  (values (Var x) (dict-ref env x))]
        [(Int n)  (values (Int n) 'Integer)]
        [else (error 'type-check-atm "expected a Cvar atm, not ~a" e)]))
      
    (define/public ((type-check-stmt env) s)
      (debug 'type-check-stmt "Cvar ~a" s)
      (match s
        [(Assign (Var x) e)
         (define-values (e^ t) ((type-check-exp env) e))
         (cond [(dict-has-key? env x)
                (check-type-equal? t (dict-ref env x) s)]
               [else (dict-set! env x t)])]
        [else (error 'type-check-stmt "expected a Cvar stmt, not ~a" s)]))

    (define/public ((type-check-tail env block-env blocks) t)
      (debug 'type-check-tail "Cvar ~a ~a" t env)
      (match t
        [(Return e)
         (define-values (e^ t) ((type-check-exp env) e))
         t]
        [(Seq s t)
         ((type-check-stmt env) s)
         ((type-check-tail env block-env blocks) t)]
        [else (error 'type-check-tail "expected a Cvar tail, not ~a" t)]))

    (define/public (type-check-program p)
      (match p
        [(CProgram info blocks)
         (define env (make-hash))
         (define block-env (make-hash))
         (define t ((type-check-tail env block-env blocks)
                    (dict-ref blocks 'start)))
         (unless (type-equal? t 'Integer)
           (error "return type of program must be Integer, not" t))
         (define locals-types (for/list ([(x t) (in-dict env)])
                                (cons x t)))
         (define new-info (dict-set info 'locals-types locals-types))
         (CProgram new-info blocks)]
        [else (error 'type-check-program "expected a C program, not ~a" p)]))
    ))

(define (type-check-Cvar p)
  (send (new type-check-Cvar-class) type-check-program p))
